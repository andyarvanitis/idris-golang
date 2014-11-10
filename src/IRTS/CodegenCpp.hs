{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module IRTS.CodegenCpp (codegenCpp) where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System hiding (getDataDir)
import IRTS.CodegenCommon
import IRTS.AST
import IRTS.CodegenGeneric
import IRTS.CodegenFFI
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
  let info = CompileCpp True
  let bytecode = map toBC definitions
  let decls = concatMap toDecl (map fst bytecode)
  let cpp = concatMap (toCpp info) bytecode
  let (header, rt) = ("", "")
  path <- getDataDir
  let cppout = (  T.pack (headers includes)
                  `T.append` namespaceBegin
                  `T.append` T.pack decls
                  `T.append` T.concat (map (compile info) cpp)
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
  [ ASTIdent $ "void " ++ translateName name,
    ASTFunction fnParams (
      ASTSeq $ ASTAlloc (Just baseType) myoldbase Nothing
               : ASTPreOp "(void)" mkMyOldbase
               : map (translateBC info)bc
    )
  ]

translateReg :: Reg -> ASTNode
translateReg reg =
  case reg of
    RVal -> mkRet
    Tmp  -> ASTRaw "//TMPREG"
    L n  -> mkLoc n
    T n  -> mkTop n

-------------------------------------------------------------------------------
instance CompileInfo CompileCpp where
-------------------------------------------------------------------------------
  mkAssign _ r1 r2 = ASTAssign (translateReg r1) (translateReg r2)

  mkAssignConst _ r c = ASTAssign (translateReg r) (mkBox' $ translateConstant c)

  mkAddTop info n = case n of
                      0 -> ASTNoop
                      _ -> ASTBinOp "+=" mkStacktop (ASTNum (ASTInt n))

  mkNullAssign _ r = ASTAssign (translateReg r) mkNull

  mkVmCall _ n = mkCall "vm_call" [mkVm, ASTIdent (translateName n), mkMyOldbase]

  mkVmTailCall _ n = mkCall "vm_tailcall" [mkVm, ASTIdent (translateName n), mkOldbase]

  mkForeign info reg n args ret =
    case n of
      "fileOpen" -> let [(_, name),(_, mode)] = args in
                    ASTAssign (translateReg reg)
                              (mkBox managedPtrTy $ mkCall "fileOpen" [mkUnbox stringTy $ translateReg name,
                                                                         mkUnbox stringTy $ translateReg mode])
      "fileClose" -> let [(_, fh)] = args in
                     ASTAssign (translateReg reg) (mkCall "fileClose" [mkUnbox managedPtrTy $ translateReg fh])

      "fputStr" -> let [(_, fh),(_, str)] = args in
                   ASTAssign (translateReg reg) (mkCall "fputStr" [mkUnbox managedPtrTy $ translateReg fh,
                                                                    mkUnbox stringTy $ translateReg str])
      "fileEOF" -> let [(_, fh)] = args in
                   ASTAssign (translateReg reg) (mkBox intTy $ mkCall "fileEOF" [mkUnbox managedPtrTy $ translateReg fh])

      "fileError" -> let [(_, fh)] = args in
                     ASTAssign (translateReg reg) (mkBox intTy $ mkCall "fileError" [mkUnbox managedPtrTy $ translateReg fh])

      "isNull" -> let [(_, arg)] = args in
                  ASTAssign (translateReg reg) (mkBox boolTy $ mkEq (translateReg arg) mkNull)

      "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                    ASTAssign (translateReg reg) (mkEq (translateReg lhs) (translateReg rhs))

      "getenv" -> let [(_, arg)] = args in
                  ASTAssign (translateReg reg) (mkBox stringTy $ mkCall "getenv" [mkMeth (mkUnbox stringTy $ translateReg arg) "c_str" []])

      _ -> ASTAssign (translateReg reg) (let callexpr = ASTFFI n (map generateWrapper args) in
                                         case ret of
                                           FUnit -> ASTBinOp "," mkNull callexpr
                                           _     -> mkBox (T.unpack . (compile info) $ foreignToBoxed ret) $ callexpr)
      where
        generateWrapper :: (FType, Reg) -> ASTNode
        generateWrapper (ty, reg) =
          case ty of
            FFunction aty rty -> mkCall "LAMBDA_WRAPPER" [translateReg reg, cType aty, cType rty]
            FFunctionIO -> error "FFunctionIO not supported yet"
            _ -> mkUnbox (T.unpack . (compile info) $ foreignToBoxed ty) $ translateReg reg

        cType :: FType -> ASTNode
        cType (FArith (ATInt ITNative))       = ASTIdent "int"
        cType (FArith (ATInt ITChar))         = ASTIdent "char"
        cType (FArith (ATInt ITBig))          = ASTIdent "long long"
        cType (FArith (ATInt (ITFixed IT8)))  = ASTIdent "int8_t"
        cType (FArith (ATInt (ITFixed IT16))) = ASTIdent "int16_t"
        cType (FArith (ATInt (ITFixed IT32))) = ASTIdent "int32_t"
        cType (FArith (ATInt (ITFixed IT64))) = ASTIdent "int64_t"
        cType FString = ASTIdent "string"
        cType FUnit = ASTIdent "void"
        cType FPtr = ASTIdent "void*"
        cType FManagedPtr = ASTIdent "shared_ptr<void>"
        cType (FArith ATFloat) = ASTIdent "double"
        cType FAny = ASTIdent "void*"
        cType (FFunction a b) = ASTList [cType a, cType b]

        foreignToBoxed :: FType -> ASTNode
        foreignToBoxed (FArith (ATInt ITNative))       = ASTIdent intTy
        foreignToBoxed (FArith (ATInt ITChar))         = ASTIdent charTy
        foreignToBoxed (FArith (ATInt ITBig))          = ASTIdent bigIntTy
        foreignToBoxed (FArith (ATInt (ITFixed IT8)))  = ASTIdent (wordTy 8)
        foreignToBoxed (FArith (ATInt (ITFixed IT16))) = ASTIdent (wordTy 16)
        foreignToBoxed (FArith (ATInt (ITFixed IT32))) = ASTIdent (wordTy 32)
        foreignToBoxed (FArith (ATInt (ITFixed IT64))) = ASTIdent (wordTy 64)
        foreignToBoxed FString = ASTIdent stringTy
        -- foreignToBoxed FUnit = ASTIdent "void"
        foreignToBoxed FPtr = ASTIdent ptrTy
        foreignToBoxed FManagedPtr = ASTIdent managedPtrTy
        foreignToBoxed (FArith ATFloat) = ASTIdent floatTy
        foreignToBoxed FAny = ASTIdent ptrTy
        -- foreignToBoxed (FFunction a b) = ASTList [cType a, cType b]

  mkTopBase _ 0  = ASTAssign mkStacktop mkStackbase
  mkTopBase _ n  = ASTAssign mkStacktop (mkAdd mkStackbase (ASTNum (ASTInt n)))

  mkBaseTop _ 0 = ASTAssign mkStackbase mkStacktop
  mkBaseTop _ n = ASTAssign mkStackbase (mkAdd mkStacktop (ASTNum (ASTInt n)))

  mkStoreOld _ = ASTAssign mkMyOldbase mkStackbase

  mkSlide _ n = mkCall "slide" [mkVm, ASTNum (ASTInt n)]

  mkRebase _ = ASTAssign mkStackbase mkOldbase

  mkReserve _ n = mkCall "reserve" [mkVm, mkAdd mkStacktop (ASTNum $ ASTInt n)]

  mkMakeCon info r t rs = 
    ASTAssign (translateReg r) (mkBox conTy $ ASTList $ ASTNum (ASTInt t) : args rs)
      where
        args [] = []
        args xs = [ASTList (map translateReg xs)]

  mkConstCase info reg cases def =
    ASTCond $ (
      map (unboxedBinOp (mkEq) (translateReg reg) . translateConstant *** prepBranch) cases
    ) ++ (maybe [] ((:[]) . ((,) ASTNoop) . prepBranch) def)
      where
        prepBranch :: [BC] -> ASTNode
        prepBranch bc = ASTSeq $ map (translateBC info) bc

        unboxedBinOp :: (ASTNode -> ASTNode -> ASTNode) -> ASTNode -> ASTNode -> ASTNode
        unboxedBinOp f l r = f (mkUnbox (unboxedType r) l) r

  mkCase info safe reg cases def =
    ASTSwitch (tag safe $ translateReg reg) (
      map ((ASTNum . ASTInt) *** prepBranch) cases
    ) (fmap prepBranch def)
      where
        tag :: Bool -> ASTNode -> ASTNode
        tag True  = mkCTag
        tag False = mkTag

        prepBranch :: [BC] -> ASTNode
        prepBranch bc = ASTSeq $ map (translateBC info) bc

        mkTag expr =
          (ASTTernary (mkIsCon expr) (
            ASTProj (mkUnbox conTy expr) "tag"
          ) (ASTNum (ASTInt $ negate 1)))

        mkCTag :: ASTNode -> ASTNode
        mkCTag expr = ASTProj (mkUnbox conTy expr) "tag"

  mkProject _ reg loc 0  = ASTNoop
  mkProject _ reg loc ar = mkCall "project" [mkVm, translateReg reg, ASTNum (ASTInt loc), ASTNum (ASTInt ar)]

  mkOp _ reg oper args = ASTAssign (translateReg reg) (mkOp' oper)
    where
      mkOp' :: PrimFn -> ASTNode
      mkOp' op =
        case op of
          LNoOp -> translateReg (last args)

          (LZExt sty dty) -> boxedIntegral dty $ unboxedIntegral sty (last args)

          (LPlus ty)  -> mkNumBinOp ty mkAdd      lhs rhs
          (LMinus ty) -> mkNumBinOp ty mkSubtract lhs rhs
          (LTimes ty) -> mkNumBinOp ty mkMultiply lhs rhs
          (LSDiv ty)  -> mkNumBinOp ty mkDivide   lhs rhs
          (LSRem ty)  -> mkNumBinOp ty mkModulo   lhs rhs

          (LEq ty)  -> mkNumCompOp ty mkEq            lhs rhs
          (LSLt ty) -> mkNumCompOp ty mkLessThan      lhs rhs
          (LSLe ty) -> mkNumCompOp ty mkLessThanEq    lhs rhs
          (LSGt ty) -> mkNumCompOp ty mkGreaterThan   lhs rhs
          (LSGe ty) -> mkNumCompOp ty mkGreaterThanEq lhs rhs

          (LTrunc ITNative (ITFixed IT8))        -> mkTrunc intTy        8  "0xFFu"
          (LTrunc (ITFixed IT16) (ITFixed IT8))  -> mkTrunc (wordTy 16)  8  "0xFFu"
          (LTrunc (ITFixed IT32) (ITFixed IT16)) -> mkTrunc (wordTy 32) 16  "0xFFFFu"
          (LTrunc (ITFixed IT64) (ITFixed IT32)) -> mkTrunc (wordTy 64) 32  "0xFFFFFFFFu"
          (LTrunc ITBig (ITFixed IT64))          -> mkTrunc bigIntTy    64  "0xFFFFFFFFFFFFFFFFu"

          (LTrunc ITBig ITNative) -> mkBox intTy $ mkCast (intTy ++ "::type") (mkUnbox bigIntTy $ translateReg arg)

          (LLSHR ty@(ITFixed _)) -> mkOp' (LASHR ty)
          (LLt ty@(ITFixed _))   -> mkOp' (LSLt (ATInt ty))
          (LLe ty@(ITFixed _))   -> mkOp' (LSLe (ATInt ty))
          (LGt ty@(ITFixed _))   -> mkOp' (LSGt (ATInt ty))
          (LGe ty@(ITFixed _))   -> mkOp' (LSGe (ATInt ty))
          (LUDiv ty@(ITFixed _)) -> mkOp' (LSDiv (ATInt ty))

          (LAnd ty)   -> boxedIntegral ty $ mkBitAnd (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)
          (LOr ty)    -> boxedIntegral ty $ mkBitOr  (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)
          (LXOr ty)   -> boxedIntegral ty $ mkBitXor (unboxedIntegral ty lhs) (unboxedIntegral ty rhs)
          (LSHL ty)   -> boxedIntegral ty $ mkBitShl (unboxedIntegral ty lhs) (mkAsIntegral $ translateReg rhs)
          (LASHR ty)  -> boxedIntegral ty $ mkBitShr (unboxedIntegral ty lhs) (mkAsIntegral $ translateReg rhs)
          (LCompl ty) -> boxedIntegral ty $ mkBitCompl (unboxedIntegral ty arg)

          LStrConcat -> mkBox stringTy $ mkAdd (unboxedString lhs) (unboxedString rhs)
          LStrEq     -> mkBox boolTy $ mkEq (unboxedString lhs) (unboxedString rhs)
          LStrLt     -> mkBox boolTy $ mkLessThan (unboxedString lhs) (unboxedString rhs)
          LStrLen    -> mkBox intTy $ mkCast (intTy ++ "::type") (strLen (mkUnbox stringTy $ translateReg arg)) -- TODO: int size 64?

          (LStrInt ITNative)     -> mkBox intTy $ mkCall "stoi" [mkUnbox stringTy $ translateReg arg]
          (LIntStr ITNative)     -> mkBox stringTy $ mkAsString $ translateReg arg
          (LSExt ITNative ITBig) -> mkBox bigIntTy $ mkUnbox intTy $ translateReg arg
          (LIntStr ITBig)        -> mkBox stringTy $ mkAsString $ translateReg arg
          (LStrInt ITBig)        -> mkBox bigIntTy $ mkCall "stoll" [mkUnbox stringTy $ translateReg arg]
          LFloatStr              -> mkBox stringTy $ mkAsString $ translateReg arg
          LStrFloat              -> mkBox floatTy $ mkCall "stod" [mkUnbox stringTy $ translateReg arg]
          (LIntFloat ITNative)   -> mkBox floatTy $ mkUnbox intTy $ translateReg arg
          (LFloatInt ITNative)   -> mkBox intTy $ mkUnbox floatTy $ translateReg arg
          (LChInt ITNative)      -> mkBox intTy $ mkUnbox charTy $ translateReg arg
          (LIntCh ITNative)      -> mkBox charTy $ mkUnbox intTy $ translateReg arg

          LFExp   -> floatfn "exp"   arg
          LFLog   -> floatfn "log"   arg
          LFSin   -> floatfn "sin"   arg
          LFCos   -> floatfn "cos"   arg
          LFTan   -> floatfn "tan"   arg
          LFASin  -> floatfn "asin"  arg
          LFACos  -> floatfn "acos"  arg
          LFATan  -> floatfn "atan"  arg
          LFSqrt  -> floatfn "sqrt"  arg
          LFFloor -> floatfn "floor" arg
          LFCeil  -> floatfn "ceil"  arg

          LStrCons -> mkBox stringTy $ mkAdd (mkAsString $ translateReg lhs) (unboxedString rhs)

          LStrHead -> let str = unboxedString arg in
                      ASTTernary (mkAnd (translateReg arg) (ASTPreOp "!" (mkMeth str "empty" [])))
                                (mkBox charTy $ mkCall "utf8_head" [str])
                                mkNull

          LStrRev   -> mkBox stringTy $ mkCall "reverse" [mkUnbox stringTy $ translateReg arg]

          LStrIndex -> mkBox charTy $ mkCall "char32_from_utf8_string" [unboxedString lhs,
                                                                        mkAsIntegral $ translateReg rhs]

          LStrTail  -> let str = unboxedString arg in
                       ASTTernary (mkAnd (translateReg arg) (mkGreaterThan (strLen str) mkOne))
                                 (mkBox stringTy $ mkCall "utf8_tail" [str])
                                 (mkBox stringTy $ ASTString "")

          LReadStr    -> mkBox stringTy $ mkCall "freadStr" [mkUnbox managedPtrTy $ translateReg arg]
          LSystemInfo -> mkBox stringTy $ mkCall "systemInfo"  [translateReg arg]
          LNullPtr    -> mkNull

          _ -> ASTError $ "Not implemented: " ++ show op

          where
            (lhs:rhs:_) = args
            (arg:_) = args

            mkNumBinOp ty f lhs rhs = boxedNum ty $ f (unboxedNum ty lhs) (unboxedNum ty rhs)
            mkNumCompOp ty f lhs rhs = mkBox boolTy $ f (unboxedNum ty lhs) (unboxedNum ty rhs)

            mkTrunc src dst mask = 
                mkBox (wordTy dst) $ mkBitAnd (mkUnbox src $ translateReg arg) (ASTRaw mask)

            strLen s = mkMeth s "length" []

            unboxedString reg = mkUnbox stringTy (translateReg reg)

            boxedNum ty expr = mkBox (arithTy ty) expr
            unboxedNum ty reg = mkUnbox (arithTy ty) (translateReg reg)

            boxedIntegral ty expr = mkBox (arithTy (ATInt ty)) expr
            unboxedIntegral ty reg = mkUnbox (arithTy (ATInt ty)) (translateReg reg)

            arithTy (ATInt ITNative)       = intTy
            arithTy (ATInt ITBig)          = bigIntTy
            arithTy (ATInt ITChar)         = charTy
            arithTy (ATFloat)              = floatTy
            arithTy (ATInt (ITFixed IT8))  = wordTy 8
            arithTy (ATInt (ITFixed IT16)) = wordTy 16
            arithTy (ATInt (ITFixed IT32)) = wordTy 32
            arithTy (ATInt (ITFixed IT64)) = wordTy 64
            arithTy (ty)                   = "UNKNOWN TYPE: " ++ show ty

            floatfn fn r = mkBox floatTy $ mkCall fn  [mkUnbox floatTy $ translateReg r]

  mkError _ = ASTError

  compileAlloc info indent (ASTAlloc typename name val) =
    case val of Nothing   -> typ `T.append` T.pack name
                Just expr -> typ `T.append` T.pack name `T.append` " = " `T.append` compile' info indent expr
                where
                  typ = case typename of Nothing -> T.pack "auto "
                                         Just t  -> T.pack (t ++ " ")

  compileError info indent (ASTError exc) = compile info (mkCall "puts" [ASTString exc])

-------------------------------------------------------------------------------

vm        = "vm"
baseType  = "IndexType"
oldbase   = "oldbase"
myoldbase = "myoldbase"

mkVm        = ASTIdent vm
mkStack     = ASTPtrProj mkVm "valstack"
mkCallstack = ASTPtrProj mkVm "callstack"
mkStackbase = ASTPtrProj mkVm "valstack_base"
mkStacktop  = ASTPtrProj mkVm "valstack_top"
mkRet       = ASTPtrProj mkVm "ret"
mkOldbase   = ASTIdent oldbase
mkMyOldbase = ASTIdent myoldbase
mkNull      = ASTIdent nullptr

mkLoc 0 = ASTIndex mkStack mkStackbase
mkLoc n = ASTIndex mkStack (mkAdd mkStackbase (ASTNum (ASTInt n)))

mkTop 0 = ASTIndex mkStack mkStacktop
mkTop n = ASTIndex mkStack (mkAdd mkStacktop (ASTNum (ASTInt n)))

mkPush args = ASTApp (ASTProj mkCallstack "push") args
mkPop       = ASTBinOp ";" (mkMeth mkCallstack "top" []) (mkMeth mkCallstack "pop" [])

mkIsCon :: ASTNode -> ASTNode
mkIsCon obj = mkAnd obj (mkEq (mkPtrMeth obj "getTypeId" []) (ASTIdent "Con::typeId"))

fnParams :: [String]
fnParams = ["shared_ptr<VirtualMachine>& " ++ vm, baseType ++ " " ++ oldbase]

mkBox' :: ASTNode -> ASTNode
mkBox' obj = mkBox (unboxedType obj) obj

mkUnbox :: String -> ASTNode -> ASTNode
mkUnbox typ obj = ASTApp (ASTIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: ASTNode -> String
unboxedType e = case e of
                  (ASTString _)                     -> stringTy
                  (ASTNum (ASTFloat _))              -> floatTy
                  (ASTNum (ASTInteger (ASTBigInt _))) -> bigIntTy
                  (ASTNum _)                        -> intTy
                  (ASTChar _)                       -> charTy
                  (ASTWord (ASTWord8 _))             -> wordTy 8
                  (ASTWord (ASTWord16 _))            -> wordTy 16
                  (ASTWord (ASTWord32 _))            -> wordTy 32
                  (ASTWord (ASTWord64 _))            -> wordTy 64
                  _                                -> ""

mkBox :: String -> ASTNode -> ASTNode
mkBox typ obj = case typ of
                       "" -> mkCall "box" [obj]
                       _  -> mkCall ("box" ++ "<" ++ typ ++ ">") [obj]

mkAsString :: ASTNode -> ASTNode
mkAsString obj = mkPtrMeth obj "asString" []

mkAsIntegral :: ASTNode -> ASTNode
mkAsIntegral obj = mkPtrMeth obj "asIntegral" []

mkCast :: String -> ASTNode -> ASTNode
mkCast typ expr = mkCall ("static_cast" ++ "<" ++ typ ++ ">") [expr]

nullptr      = "nullptr"
intTy        = "Int"
bigIntTy     = "BigInt"
floatTy      = "Float"
stringTy     = "String"
charTy       = "Char"
managedPtrTy = "ManagedPtr"
ptrTy        = "Ptr"
conTy        = "Con"
boolTy       = intTy

wordTy :: Int -> String
wordTy n = PF.printf "Word%d" n
