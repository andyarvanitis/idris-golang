{-# LANGUAGE CPP #-}

module IRTS.CodegenCpp (codegenCpp) where

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System hiding (getDataDir)
import IRTS.CodegenCommon
import IRTS.IL.AST
import IRTS.CodegenIL
import Idris.Core.TT
import Util.System

import Numeric
import Data.Char
import Data.List (intercalate)
import System.Process
import System.Exit
import System.IO
import System.Directory
import Control.Monad.State
import Control.Arrow

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Printf as PF

import Paths_idris_cpp

-- TODO: better way to do this?
#if defined(MACOSX) || defined(FREEBSD)
ccStandard = "-std=c++11 -stdlib=libc++"
libStandard = "-lc++"
#else
ccStandard = "-std=c++11 -stdlib=libstdc++"
libStandard = "-lstdc++"
#endif

data CompileCpp = CompileCpp Bool -- TODO: just a placeholder

codegenCpp :: CodeGenerator
codegenCpp ci =
  codegenCpp_all (simpleDecls ci)
                 (outputType ci)
                 (outputFile ci)
                 (includes ci)
                 (concatMap mkObj (compileObjs ci))
                 (concatMap mkLib (compileLibs ci) ++
                     concatMap incdir (importDirs ci))
                 (concatMap mkFlag (compilerFlags ci))
                 (debugLevel ci)
    where
      mkObj f = f ++ " "
      mkLib l = "-l" ++ l ++ " "
      mkFlag l = l ++ " "
      incdir i = "-I" ++ i ++ " "

codegenCpp_all ::
     [(Name, SDecl)] -> -- declarations/definitions
     OutputType ->      -- output type
     FilePath ->        -- output file name
     [FilePath] ->      -- include files
     String ->          -- extra object files`as
     String ->          -- libraries
     String ->          -- extra compiler flags
     DbgLevel ->        -- debug level
     IO ()

codegenCpp_all definitions outputType filename includes objs libs flags dbg = do
  let bytecode = map toBC definitions
  let decls = concatMap toDecl (map fst bytecode)
  let cpp = concatMap (toCpp (CompileCpp True)) bytecode
  let (header, rt) = ("", "")
  path <- getDataDir
  let cppout = (  T.pack (headers includes)
                  `T.append` namespaceBegin
                  `T.append` T.pack decls
                  `T.append` T.concat (map compileIL cpp)
                  `T.append` namespaceEnd
               )
  case outputType of
    Raw -> TIO.writeFile filename cppout
    _ -> do (tmpn, tmph) <- tempfile
            hPutStr tmph (T.unpack cppout)
            hFlush tmph
            hClose tmph
            comp <- getCC
            libFlags <- getLibFlags
            incFlags <- getIncFlags
            let cc = comp ++ " " ++
                    ccStandard ++ " " ++
                    ccDbg dbg ++ " " ++
                    ccFlags ++
                    " -I " ++ path ++ "/include" ++
                    " -I. " ++ objs ++ " -x c++ " ++
                    (if (outputType == Executable) then "" else " -c ") ++
                    " " ++ tmpn ++
                    " " ++ libStandard ++ " " ++
                    " -L " ++ path ++ "/lib" ++
                    " " ++ libRuntime ++ " " ++
                    " " ++ libFlags ++
                    " " ++ incFlags ++
                    " " ++ libs ++
                    " " ++ flags ++
                    " -o " ++ filename
            exit <- system cc
            when (exit /= ExitSuccess) $
              putStrLn ("FAILURE: " ++ cc)
    where
      headers xs = concatMap (\h -> let header = case h of ('<':_) -> h
                                                           _ -> "\"" ++ h ++ "\"" in
                                    "#include " ++ header ++ "\n")
                             (xs ++ ["idris_runtime.h"])

      debug TRACE = "#define IDRIS_TRACE\n\n"
      debug _ = ""

      -- We're using signed integers now. Make sure we get consistent semantics
      -- out of them from cc. See e.g. http://thiemonagel.de/2010/01/signed-integer-overflow/
      ccFlags = " -fwrapv -fno-strict-overflow"

      libRuntime = "-lidris_cpp_rts"

      ccDbg DEBUG = "-g"
      ccDbg TRACE = "-O2"
      ccDbg _ = "-O2 -DNDEBUG -ftree-vectorize -fno-rtti -fno-exceptions -fomit-frame-pointer"

      toDecl :: Name -> String
      toDecl f = "void " ++ translateName f ++ "(" ++ (intercalate ", " fnParams) ++ ");\n"

      namespaceBegin :: T.Text
      namespaceBegin = T.pack "namespace idris {\n"

      namespaceEnd :: T.Text
      namespaceEnd = T.pack "} // namespace idris\n"

toCpp info (name, bc) =
  [ ILIdent $ "void " ++ translateName name,
    ILFunction fnParams (
      ILSeq $ ILAlloc (Just baseType) myoldbase Nothing
               : ILPreOp "(void)" mkMyOldbase
               : map (translateBC info)bc
    )
  ]

translateReg :: Reg -> ILExpr
translateReg reg =
  case reg of
    RVal -> mkRet
    Tmp  -> ILRaw "//TMPREG"
    L n  -> mkLoc n
    T n  -> mkTop n

-------------------------------------------------------------------------------
instance CompileInfo CompileCpp where
-------------------------------------------------------------------------------
  mkAssign _ r1 r2 = ILAssign (translateReg r1) (translateReg r2)

  mkAssignConst _ r c = ILAssign (translateReg r) (mkBox' $ translateConstant c)

  mkAddTop info n = case n of
                      0 -> ILNoop
                      _ -> ILBinOp "+=" mkStacktop (ILNum (ILInt n))

  mkNull _ r = ILAssign (translateReg r) ILNull

  mkCall _ n = mkILCall "vm_call" [mkVM, ILIdent (translateName n), mkMyOldbase]

  mkTailCall _ n = mkILCall "vm_tailcall" [mkVM, ILIdent (translateName n), mkOldbase]

  mkForeign _ reg n args ret =
    case n of
      "fileOpen" -> let [(_, name),(_, mode)] = args in
                    ILAssign (translateReg reg)
                              (mkBox managedPtrTy $ mkILCall "fileOpen" [mkUnbox stringTy $ translateReg name,
                                                                         mkUnbox stringTy $ translateReg mode])
      "fileClose" -> let [(_, fh)] = args in
                     ILAssign (translateReg reg) (mkILCall "fileClose" [mkUnbox managedPtrTy $ translateReg fh])

      "fputStr" -> let [(_, fh),(_, str)] = args in
                   ILAssign (translateReg reg) (mkILCall "fputStr" [mkUnbox managedPtrTy $ translateReg fh,
                                                                    mkUnbox stringTy $ translateReg str])
      "fileEOF" -> let [(_, fh)] = args in
                   ILAssign (translateReg reg) (mkBox intTy $ mkILCall "fileEOF" [mkUnbox managedPtrTy $ translateReg fh])

      "fileError" -> let [(_, fh)] = args in
                     ILAssign (translateReg reg) (mkBox intTy $ mkILCall "fileError" [mkUnbox managedPtrTy $ translateReg fh])

      "isNull" -> let [(_, arg)] = args in
                  ILAssign (translateReg reg) (mkBox boolTy $ mkILEq (translateReg arg) ILNull)

      "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                    ILAssign (translateReg reg) (mkILEq (translateReg lhs) (translateReg rhs))

      "getenv" -> let [(_, arg)] = args in
                  ILAssign (translateReg reg) (mkBox stringTy $ mkILCall "getenv" [mkILMeth (mkUnbox stringTy $ translateReg arg) "c_str" []])

      _ -> ILAssign (translateReg reg) (let callexpr = ILFFI n (map generateWrapper args) in
                                         case ret of
                                           FUnit -> ILBinOp "," ILNull callexpr
                                           _     -> mkBox (T.unpack . compileIL $ foreignToBoxed ret) $ callexpr)
      where
        generateWrapper :: (FType, Reg) -> ILExpr
        generateWrapper (ty, reg) =
          case ty of
            FFunction aty rty -> mkILCall "LAMBDA_WRAPPER" [translateReg reg, cType aty, cType rty]
            FFunctionIO -> error "FFunctionIO not supported yet"
            _ -> mkUnbox (T.unpack . compileIL $ foreignToBoxed ty) $ translateReg reg

        cType :: FType -> ILExpr
        cType (FArith (ATInt ITNative))       = ILIdent "int"
        cType (FArith (ATInt ITChar))         = ILIdent "char"
        cType (FArith (ATInt ITBig))          = ILIdent "long long"
        cType (FArith (ATInt (ITFixed IT8)))  = ILIdent "int8_t"
        cType (FArith (ATInt (ITFixed IT16))) = ILIdent "int16_t"
        cType (FArith (ATInt (ITFixed IT32))) = ILIdent "int32_t"
        cType (FArith (ATInt (ITFixed IT64))) = ILIdent "int64_t"
        cType FString = ILIdent "string"
        cType FUnit = ILIdent "void"
        cType FPtr = ILIdent "void*"
        cType FManagedPtr = ILIdent "shared_ptr<void>"
        cType (FArith ATFloat) = ILIdent "double"
        cType FAny = ILIdent "void*"
        cType (FFunction a b) = ILList [cType a, cType b]

        foreignToBoxed :: FType -> ILExpr
        foreignToBoxed (FArith (ATInt ITNative))       = ILIdent intTy
        foreignToBoxed (FArith (ATInt ITChar))         = ILIdent charTy
        foreignToBoxed (FArith (ATInt ITBig))          = ILIdent bigintTy
        foreignToBoxed (FArith (ATInt (ITFixed IT8)))  = ILIdent (wordTy 8)
        foreignToBoxed (FArith (ATInt (ITFixed IT16))) = ILIdent (wordTy 16)
        foreignToBoxed (FArith (ATInt (ITFixed IT32))) = ILIdent (wordTy 32)
        foreignToBoxed (FArith (ATInt (ITFixed IT64))) = ILIdent (wordTy 64)
        foreignToBoxed FString = ILIdent stringTy
        -- foreignToBoxed FUnit = ILIdent "void"
        foreignToBoxed FPtr = ILIdent ptrTy
        foreignToBoxed FManagedPtr = ILIdent managedPtrTy
        foreignToBoxed (FArith ATFloat) = ILIdent floatTy
        foreignToBoxed FAny = ILIdent ptrTy
        -- foreignToBoxed (FFunction a b) = ILList [cType a, cType b]

  mkTopBase _ 0  = ILAssign mkStacktop mkStackbase
  mkTopBase _ n  = ILAssign mkStacktop (mkILAdd mkStackbase (ILNum (ILInt n)))

  mkBaseTop _ 0 = ILAssign mkStackbase mkStacktop
  mkBaseTop _ n = ILAssign mkStackbase (mkILAdd mkStacktop (ILNum (ILInt n)))

  mkStoreOld _ = ILAssign mkMyOldbase mkStackbase

  mkSlide _ n = mkILCall "slide" [mkVM, ILNum (ILInt n)]

  mkRebase _ = ILAssign mkStackbase mkOldbase

  mkReserve _ n = mkILCall "reserve" [mkVM, mkILAdd mkStacktop (ILNum $ ILInt n)]

  mkMakeCon info r t rs = 
    ILAssign (translateReg r) (mkBox conTy $ ILList $ ILNum (ILInt t) : args rs)
      where
        args [] = []
        args xs = [ILList (map translateReg xs)]

  mkConstCase info reg cases def =
    ILCond $ (
      map (unboxedBinOp (mkILEq) (translateReg reg) . translateConstant *** prepBranch) cases
    ) ++ (maybe [] ((:[]) . ((,) ILNoop) . prepBranch) def)
      where
        prepBranch :: [BC] -> ILExpr
        prepBranch bc = ILSeq $ map (translateBC info) bc

        unboxedBinOp :: (ILExpr -> ILExpr -> ILExpr) -> ILExpr -> ILExpr -> ILExpr
        unboxedBinOp f l r = f (mkUnbox (unboxedType r) l) r

  mkCase info safe reg cases def =
    ILSwitch (tag safe $ translateReg reg) (
      map ((ILNum . ILInt) *** prepBranch) cases
    ) (fmap prepBranch def)
      where
        tag :: Bool -> ILExpr -> ILExpr
        tag True  = mkCTag
        tag False = mkTag

        prepBranch :: [BC] -> ILExpr
        prepBranch bc = ILSeq $ map (translateBC info) bc

        mkTag expr =
          (ILTernary (expr `mkILInstanceOf` "Con::typeId") (
            ILProj (mkUnbox conTy expr) "tag"
          ) (ILNum (ILInt $ negate 1)))

        mkCTag :: ILExpr -> ILExpr
        mkCTag expr = ILProj (mkUnbox conTy expr) "tag"

  mkProject _ reg loc 0  = ILNoop
  mkProject _ reg loc ar = mkILCall "project" [mkVM, translateReg reg, ILNum (ILInt loc), ILNum (ILInt ar)]

  mkOp _ reg oper args = ILAssign (translateReg reg) (mkOp' oper)
    where
      mkOp' :: PrimFn -> ILExpr
      mkOp' op =
        case op of
          LNoOp -> translateReg (last args)

          (LZExt sty dty) -> boxedIntegral dty $ unboxedIntegral sty (last args)

          (LPlus ty) -> boxedNum ty $ mkILAdd (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LMinus ty) -> boxedNum ty $ mkILSubtract (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LTimes ty) -> boxedNum ty $ mkILMultiply (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSDiv ty) -> boxedNum ty $ mkILDivide (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSRem ty) -> boxedNum ty $ mkILModulo (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LEq ty) -> boxedNum ty $ mkILEq (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSLt ty) -> boxedNum ty $ mkILLessThan (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSLe ty) -> boxedNum ty $ mkILLessThanEq (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSGt ty) -> boxedNum ty $ mkILGreaterThan (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LSGe ty) -> boxedNum ty $ mkILGreaterThanEq (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LTrunc ITNative (ITFixed IT8)) -> mkBox (wordTy 8) $ 
                                             mkILBitAnd (mkUnbox intTy $ translateReg arg) (ILRaw "0xFFu")

          (LTrunc (ITFixed IT16) (ITFixed IT8)) -> mkBox (wordTy 8) $ 
                                                   mkILBitAnd (mkUnbox (wordTy 16) $ translateReg arg) (ILRaw "0xFFu")

          (LTrunc (ITFixed IT32) (ITFixed IT16)) -> mkBox (wordTy 16) $ 
                                                    mkILBitAnd (mkUnbox (wordTy 32) $ translateReg arg) (ILRaw "0xFFFFu")

          (LTrunc (ITFixed IT64) (ITFixed IT32)) -> mkBox (wordTy 32) $
                                                    mkILBitAnd (mkUnbox (wordTy 64) $ translateReg arg) (ILRaw "0xFFFFFFFFu")

          (LTrunc ITBig (ITFixed IT64)) -> mkBox (wordTy 64) $ 
                                           mkILBitAnd (mkUnbox bigintTy $ translateReg arg) (ILRaw "0xFFFFFFFFFFFFFFFFu")

          (LTrunc ITBig ITNative) -> mkBox intTy $ mkILStaticCast (intTy ++ "::type") (mkUnbox bigintTy $ translateReg arg)

          (LLSHR ty@(ITFixed _)) -> mkOp' (LASHR ty)
          (LLt ty@(ITFixed _))   -> mkOp' (LSLt (ATInt ty))
          (LLe ty@(ITFixed _))   -> mkOp' (LSLe (ATInt ty))
          (LGt ty@(ITFixed _))   -> mkOp' (LSGt (ATInt ty))
          (LGe ty@(ITFixed _))   -> mkOp' (LSGe (ATInt ty))
          (LUDiv ty@(ITFixed _)) -> mkOp' (LSDiv (ATInt ty))

          (LAnd ty) -> boxedIntegral ty $ mkILBitAnd (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)

          (LOr ty)  -> boxedIntegral ty $ mkILBitOr (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)

          (LXOr ty) -> boxedIntegral ty $ mkILBitXor (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)

          (LSHL ty) -> boxedIntegral ty $ mkILBitShl (unboxedIntegral ty lhs) (mkAsIntegral $ translateReg rhs)

          (LASHR ty)  -> boxedIntegral ty $ mkILBitShr (unboxedIntegral ty lhs) (mkAsIntegral $ translateReg rhs)

          (LCompl ty) -> boxedIntegral ty $ mkILBitCompl (unboxedIntegral ty arg)

          LStrConcat -> mkBox stringTy $ mkILAdd (unboxedString lhs) (unboxedString rhs)

          LStrEq -> mkBox boolTy $ mkILEq (unboxedString lhs) (unboxedString rhs)

          LStrLt -> mkBox boolTy $ mkILLessThan (unboxedString lhs) (unboxedString rhs)

          LStrLen -> mkBox intTy $ mkILStaticCast (intTy ++ "::type") (strLen (mkUnbox stringTy $ translateReg arg)) -- TODO: int size 64?

          (LStrInt ITNative)     -> mkBox intTy $ mkILCall "stoi" [mkUnbox stringTy $ translateReg arg]
          (LIntStr ITNative)     -> mkBox stringTy $ mkAsString $ translateReg arg
          (LSExt ITNative ITBig) -> mkBox bigintTy $ mkUnbox intTy $ translateReg arg
          (LIntStr ITBig)        -> mkBox stringTy $ mkAsString $ translateReg arg
          (LStrInt ITBig)        -> mkBox bigintTy $ mkILCall "stoll" [mkUnbox stringTy $ translateReg arg]
          LFloatStr              -> mkBox stringTy $ mkAsString $ translateReg arg
          LStrFloat              -> mkBox floatTy $ mkILCall "stod" [mkUnbox stringTy $ translateReg arg]
          (LIntFloat ITNative)   -> mkBox floatTy $ mkUnbox intTy $ translateReg arg
          (LFloatInt ITNative)   -> mkBox intTy $ mkUnbox floatTy $ translateReg arg
          (LChInt ITNative)      -> mkBox intTy $ mkUnbox charTy $ translateReg arg
          (LIntCh ITNative)      -> mkBox charTy $ mkUnbox intTy $ translateReg arg

          LFExp   -> mkBox floatTy $ mkILCall "exp"   [mkUnbox floatTy $ translateReg arg]
          LFLog   -> mkBox floatTy $ mkILCall "log"   [mkUnbox floatTy $ translateReg arg]
          LFSin   -> mkBox floatTy $ mkILCall "sin"   [mkUnbox floatTy $ translateReg arg]
          LFCos   -> mkBox floatTy $ mkILCall "cos"   [mkUnbox floatTy $ translateReg arg]
          LFTan   -> mkBox floatTy $ mkILCall "tan"   [mkUnbox floatTy $ translateReg arg]
          LFASin  -> mkBox floatTy $ mkILCall "asin"  [mkUnbox floatTy $ translateReg arg]
          LFACos  -> mkBox floatTy $ mkILCall "acos"  [mkUnbox floatTy $ translateReg arg]
          LFATan  -> mkBox floatTy $ mkILCall "atan"  [mkUnbox floatTy $ translateReg arg]
          LFSqrt  -> mkBox floatTy $ mkILCall "sqrt"  [mkUnbox floatTy $ translateReg arg]
          LFFloor -> mkBox floatTy $ mkILCall "floor" [mkUnbox floatTy $ translateReg arg]
          LFCeil  -> mkBox floatTy $ mkILCall "ceil"  [mkUnbox floatTy $ translateReg arg]

          LStrCons -> mkBox stringTy $ mkILAdd (mkAsString $ translateReg lhs) (unboxedString rhs)

          LStrHead -> let str = unboxedString arg in
                      ILTernary (mkILAnd (translateReg arg) (ILPreOp "!" (mkILMeth str "empty" [])))
                                (mkBox charTy $ mkILCall "utf8_head" [str])
                                ILNull

          LStrRev   -> mkBox stringTy $ mkILCall "reverse" [mkUnbox stringTy $ translateReg arg]

          LStrIndex -> mkBox charTy $ mkILCall "char32_from_utf8_string" [unboxedString lhs,                                                                            mkAsIntegral $ translateReg rhs]

          LStrTail  -> let str = unboxedString arg in
                       ILTernary (mkILAnd (translateReg arg) (mkILGreaterThan (strLen str) mkILOne))
                                 (mkBox stringTy $ mkILCall "utf8_tail" [str])
                                 (mkBox stringTy $ ILString "")

          LReadStr    -> mkBox stringTy $ mkILCall "freadStr" [mkUnbox managedPtrTy $ translateReg arg]
          LSystemInfo -> mkBox stringTy $ mkILCall "systemInfo"  [translateReg arg]
          LNullPtr    -> ILNull

          _ -> ILError $ "Not implemented: " ++ show op

          where
            (lhs:rhs:_) = args
            (arg:_) = args

            strLen :: ILExpr -> ILExpr
            strLen s = mkILMeth s "length" []

            unboxedNum :: ArithTy -> Reg -> ILExpr
            unboxedNum ty reg = mkUnbox (arithTy ty) (translateReg reg)

            boxedNum :: ArithTy -> ILExpr -> ILExpr
            boxedNum ty expr = mkBox (arithTy ty) expr

            unboxedIntegral :: IntTy -> Reg -> ILExpr
            unboxedIntegral ty reg = mkUnbox (arithTy (ATInt ty)) (translateReg reg)

            boxedIntegral :: IntTy -> ILExpr -> ILExpr
            boxedIntegral ty expr = mkBox (arithTy (ATInt ty)) expr

            unboxedString :: Reg -> ILExpr
            unboxedString reg = mkUnbox stringTy (translateReg reg)

            arithTy :: ArithTy -> String
            arithTy (ATInt ITNative)       = intTy
            arithTy (ATInt ITBig)          = bigintTy
            arithTy (ATInt ITChar)         = charTy
            arithTy (ATFloat)              = floatTy
            arithTy (ATInt (ITFixed IT8))  = wordTy 8
            arithTy (ATInt (ITFixed IT16)) = wordTy 16
            arithTy (ATInt (ITFixed IT32)) = wordTy 32
            arithTy (ATInt (ITFixed IT64)) = wordTy 64
            arithTy (ty)                   = "UNKNOWN TYPE: " ++ show ty

  mkError _ = ILError

vm :: String
vm = "vm"

mkVM :: ILExpr
mkVM = ILIdent vm

mkStack :: ILExpr
mkStack = ILPtrProj mkVM "valstack"

mkCallstack :: ILExpr
mkCallstack = ILPtrProj mkVM "callstack"

mkArgstack :: ILExpr
mkArgstack = ILPtrProj mkVM "argstack"

mkStackbase :: ILExpr
mkStackbase = ILPtrProj mkVM "valstack_base"

mkStacktop :: ILExpr
mkStacktop = ILPtrProj mkVM "valstack_top"

baseType :: String
baseType = "IndexType"

oldbase :: String
oldbase = "oldbase"

mkOldbase :: ILExpr
mkOldbase = ILIdent oldbase

myoldbase :: String
myoldbase = "myoldbase"

mkMyOldbase :: ILExpr
mkMyOldbase = ILIdent myoldbase

fnParams :: [String]
fnParams = ["shared_ptr<VirtualMachine>& " ++ vm, baseType ++ " " ++ oldbase]

mkRet :: ILExpr
mkRet = ILPtrProj mkVM "ret"

mkLoc :: Int -> ILExpr
mkLoc 0 = ILIndex mkStack mkStackbase
mkLoc n = ILIndex mkStack (mkILAdd mkStackbase (ILNum (ILInt n)))

mkTop :: Int -> ILExpr
mkTop 0 = ILIndex mkStack mkStacktop
mkTop n = ILIndex mkStack (mkILAdd mkStacktop (ILNum (ILInt n)))

mkPush :: [ILExpr] -> ILExpr
mkPush args = ILApp (ILProj mkCallstack "push") args

mkPop :: ILExpr
mkPop = ILBinOp ";" (mkILMeth mkCallstack "top" []) (mkILMeth mkCallstack "pop" [])

mkBox' :: ILExpr -> ILExpr
mkBox' obj = mkBox (unboxedType obj) obj

mkUnbox :: String -> ILExpr -> ILExpr
mkUnbox typ obj = ILApp (ILIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: ILExpr -> String
unboxedType e = case e of
                  (ILString _)                     -> stringTy
                  (ILNum (ILFloat _))              -> floatTy
                  (ILNum (ILInteger (ILBigInt _))) -> bigintTy
                  (ILNum _)                        -> intTy
                  (ILChar _)                       -> charTy
                  (ILWord (ILWord8 _))             -> wordTy 8
                  (ILWord (ILWord16 _))            -> wordTy 16
                  (ILWord (ILWord32 _))            -> wordTy 32
                  (ILWord (ILWord64 _))            -> wordTy 64
                  _                                -> ""

mkBox :: String -> ILExpr -> ILExpr
mkBox typ obj = case typ of
                       "" -> mkILCall "box" [obj]
                       _  -> mkILCall ("box" ++ "<" ++ typ ++ ">") [obj]

mkAsString :: ILExpr -> ILExpr
mkAsString obj = mkILPtrMeth obj "asString" []

mkAsIntegral :: ILExpr -> ILExpr
mkAsIntegral obj = mkILPtrMeth obj "asIntegral" []

intTy :: String
intTy = "Int"

bigintTy :: String
bigintTy = "BigInt"

floatTy :: String
floatTy = "Float"

stringTy :: String
stringTy = "String"

charTy :: String
charTy = "Char"

wordTy :: Int -> String
wordTy n = PF.printf "Word%d" n

managedPtrTy :: String
managedPtrTy = "ManagedPtr"

ptrTy :: String
ptrTy = "Ptr"

conTy :: String
conTy = "Con"

boolTy :: String
boolTy = intTy

