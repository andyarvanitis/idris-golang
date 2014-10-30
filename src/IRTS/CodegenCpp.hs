{-# LANGUAGE CPP #-}

module IRTS.CodegenCpp (codegenCpp) where

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System hiding (getDataDir)
import IRTS.CodegenCommon
import IRTS.Cpp.AST
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

data CompileInfo = CompileInfo { compileInfoNeedsBigInt :: Bool } -- TODO: not used right now

data CppTarget = Cpp deriving Eq

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
  let cpp = concatMap (toCpp (CompileInfo True)) bytecode
  let (header, rt) = ("", "")
  path <- getDataDir
  let cppout = (  T.pack (headers includes)
                  `T.append` namespaceBegin
                  `T.append` T.pack decls
                  `T.append` T.concat (map compileCpp cpp)
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
      ccDbg _ = "-O2"

      toDecl :: Name -> String
      toDecl f = "void " ++ translateName f ++ "(" ++ (intercalate ", " cppFUNCPARMS) ++ ");\n"

      namespaceBegin :: T.Text
      namespaceBegin = T.pack "namespace idris {\n"

      namespaceEnd :: T.Text
      namespaceEnd = T.pack "} // namespace idris\n"

toCpp info (name, bc) =
  [ CppIdent $ "void " ++ translateName name,
    CppFunction cppFUNCPARMS (
      CppSeq $ CppAlloc (Just cppBASETYPENAME) cppMYOLDBASENAME Nothing
               : CppPreOp "(void)" cppMYOLDBASE
               : map (translateBC info)bc
    )
  ]

translateReg :: Reg -> Cpp
translateReg reg =
  case reg of
    RVal -> cppRET
    Tmp  -> CppRaw "//TMPREG"
    L n  -> cppLOC n
    T n  -> cppTOP n

translateConstant :: Const -> Cpp
translateConstant (I i)                    = CppNum (CppInt i)
translateConstant (Fl f)                   = CppNum (CppFloat f)
translateConstant (Ch c)                   = CppChar (translateChar c)
translateConstant (Str s)                  = CppString $ concatMap translateChar s
translateConstant (AType (ATInt ITNative)) = CppType CppIntTy
translateConstant StrType                  = CppType CppStringTy
translateConstant (AType (ATInt ITBig))    = CppType CppIntegerTy
translateConstant (AType ATFloat)          = CppType CppFloatTy
translateConstant (AType (ATInt ITChar))   = CppType CppCharTy
translateConstant PtrType                  = CppType CppPtrTy
translateConstant Forgot                   = CppType CppForgotTy
translateConstant (BI i)                   = CppNum $ CppInteger (CppBigInt i)
translateConstant (B8 b)                   = CppWord (CppWord8 b)
translateConstant (B16 b)                  = CppWord (CppWord16 b)
translateConstant (B32 b)                  = CppWord (CppWord32 b)
translateConstant (B64 b)                  = CppWord (CppWord64 b)
translateConstant c =
  CppError $ "Unimplemented Constant: " ++ show c

translateChar :: Char -> String
translateChar ch
  | isAscii ch && isAlphaNum ch  = [ch]
  | ch `elem` [' ','_', ',','.'] = [ch]
  | otherwise                    = cppCodepoint (ord ch)

translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"

cppASSIGN :: CompileInfo -> Reg -> Reg -> Cpp
cppASSIGN _ r1 r2 = CppAssign (translateReg r1) (translateReg r2)

cppASSIGNCONST :: CompileInfo -> Reg -> Const -> Cpp
cppASSIGNCONST _ r c = CppAssign (translateReg r) (cppBOX' $ translateConstant c)

cppCALL :: CompileInfo -> Name -> Cpp
cppCALL _ n =
  CppApp (
    CppIdent "vm_call"
  ) [cppVM, CppIdent (translateName n), cppMYOLDBASE]

cppTAILCALL :: CompileInfo -> Name -> Cpp
cppTAILCALL _ n =
  CppApp (
    CppIdent "vm_tailcall"
  ) [cppVM, CppIdent (translateName n), cppOLDBASE]

cppFOREIGN :: CompileInfo -> Reg -> String -> [(FType, Reg)] -> FType -> Cpp
cppFOREIGN _ reg n args ret =
  case n of
    "fileOpen" -> let [(_, name),(_, mode)] = args in
                  CppAssign (translateReg reg)
                            (cppBOX cppManagedPtr $ cppCall "fileOpen" [cppUNBOX cppSTRING $ translateReg name,
                                                                        cppUNBOX cppSTRING $ translateReg mode])
    "fileClose" -> let [(_, fh)] = args in
                   CppAssign (translateReg reg) (cppCall "fileClose" [cppUNBOX cppManagedPtr $ translateReg fh])

    "fputStr" -> let [(_, fh),(_, str)] = args in
                 CppAssign (translateReg reg) (cppCall "fputStr" [cppUNBOX cppManagedPtr $ translateReg fh,
                                                                      cppUNBOX cppSTRING $ translateReg str])
    "fileEOF" -> let [(_, fh)] = args in
                 CppAssign (translateReg reg) (cppBOX cppINT $ cppCall "fileEOF" [cppUNBOX cppManagedPtr $ translateReg fh])

    "fileError" -> let [(_, fh)] = args in
                   CppAssign (translateReg reg) (cppBOX cppINT $ cppCall "fileError" [cppUNBOX cppManagedPtr $ translateReg fh])

    "isNull" -> let [(_, arg)] = args in
                CppAssign (translateReg reg) (cppBOX cppBOOL $ CppBinOp "==" (translateReg arg) CppNull)

    "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                  CppAssign (translateReg reg) (CppBinOp "==" (translateReg lhs) (translateReg rhs))

    "getenv" -> let [(_, arg)] = args in
                CppAssign (translateReg reg) (cppBOX cppSTRING $ cppCall "getenv" [cppMeth (cppUNBOX cppSTRING $ translateReg arg) "c_str" []])

    _ -> CppAssign (translateReg reg) (let callexpr = CppFFI n (map generateWrapper args) in
                                       case ret of
                                         FUnit -> CppBinOp "," CppNull callexpr
                                         _     -> cppBOX (T.unpack . compileCpp $ foreignToBoxed ret) $ callexpr)
    where
      generateWrapper :: (FType, Reg) -> Cpp
      generateWrapper (ty, reg) =
        case ty of
          FFunction aty rty ->
            CppApp (CppIdent $ "LAMBDA_WRAPPER") [translateReg reg, cType aty, cType rty]
          FFunctionIO -> error "FFunctionIO not supported yet"
          _ -> cppUNBOX (T.unpack . compileCpp $ foreignToBoxed ty) $ translateReg reg

      cType :: FType -> Cpp
      cType (FArith (ATInt ITNative))       = CppIdent "int"
      cType (FArith (ATInt ITChar))         = CppIdent "char"
      cType (FArith (ATInt ITBig))          = CppIdent "long long"
      cType (FArith (ATInt (ITFixed IT8)))  = CppIdent "int8_t"
      cType (FArith (ATInt (ITFixed IT16))) = CppIdent "int16_t"
      cType (FArith (ATInt (ITFixed IT32))) = CppIdent "int32_t"
      cType (FArith (ATInt (ITFixed IT64))) = CppIdent "int64_t"
      cType FString = CppIdent "string"
      cType FUnit = CppIdent "void"
      cType FPtr = CppIdent "void*"
      cType FManagedPtr = CppIdent "shared_ptr<void>"
      cType (FArith ATFloat) = CppIdent "double"
      cType FAny = CppIdent "void*"
      cType (FFunction a b) = CppList [cType a, cType b]

      foreignToBoxed :: FType -> Cpp
      foreignToBoxed (FArith (ATInt ITNative))       = CppIdent cppINT
      foreignToBoxed (FArith (ATInt ITChar))         = CppIdent cppCHAR
      foreignToBoxed (FArith (ATInt ITBig))          = CppIdent cppBIGINT
      foreignToBoxed (FArith (ATInt (ITFixed IT8)))  = CppIdent (cppWORD 8)
      foreignToBoxed (FArith (ATInt (ITFixed IT16))) = CppIdent (cppWORD 16)
      foreignToBoxed (FArith (ATInt (ITFixed IT32))) = CppIdent (cppWORD 32)
      foreignToBoxed (FArith (ATInt (ITFixed IT64))) = CppIdent (cppWORD 64)
      foreignToBoxed FString = CppIdent cppSTRING
      -- foreignToBoxed FUnit = CppIdent "void"
      foreignToBoxed FPtr = CppIdent cppPTR
      foreignToBoxed FManagedPtr = CppIdent cppManagedPtr
      foreignToBoxed (FArith ATFloat) = CppIdent cppFLOAT
      foreignToBoxed FAny = CppIdent cppPTR
      -- foreignToBoxed (FFunction a b) = CppList [cType a, cType b]

cppREBASE :: CompileInfo -> Cpp
cppREBASE _ = CppAssign cppSTACKBASE cppOLDBASE

cppSTOREOLD :: CompileInfo ->Cpp
cppSTOREOLD _ = CppAssign cppMYOLDBASE cppSTACKBASE

cppADDTOP :: CompileInfo -> Int -> Cpp
cppADDTOP info n = case n of
                     0 -> CppNoop
                     _ -> CppBinOp "+=" cppSTACKTOP (CppNum (CppInt n))

cppTOPBASE :: CompileInfo -> Int -> Cpp
cppTOPBASE _ 0  = CppAssign cppSTACKTOP cppSTACKBASE
cppTOPBASE _ n  = CppAssign cppSTACKTOP (CppBinOp "+" cppSTACKBASE (CppNum (CppInt n)))

cppBASETOP :: CompileInfo -> Int -> Cpp
cppBASETOP _ 0 = CppAssign cppSTACKBASE cppSTACKTOP
cppBASETOP _ n = CppAssign cppSTACKBASE (CppBinOp "+" cppSTACKTOP (CppNum (CppInt n)))

cppNULL :: CompileInfo -> Reg -> Cpp
cppNULL _ r = CppAssign (translateReg r) CppNull

cppERROR :: CompileInfo -> String -> Cpp
cppERROR _ = CppError

cppSLIDE :: CompileInfo -> Int -> Cpp
cppSLIDE _ 1 = CppAssign (cppLOC 0) (cppTOP 0)
cppSLIDE _ n = CppApp (CppIdent "slide") [cppVM, CppNum (CppInt n)]

cppRESERVE :: CompileInfo -> Int -> Cpp
cppRESERVE _ n = cppCall "reserve" [cppVM, CppBinOp "+" cppSTACKTOP (CppNum $ CppInt n)]

cppMKCON :: CompileInfo -> Reg -> Int -> [Reg] -> Cpp
cppMKCON info r t rs =
  CppAssign (translateReg r) (
    cppBOX cppCON $ CppList $ CppNum (CppInt t) : args rs
  )
    where
      args [] = []
      args xs = [CppList (map translateReg xs)]

cppCASE :: CompileInfo -> Bool -> Reg -> [(Int, [BC])] -> Maybe [BC] -> Cpp
cppCASE info safe reg cases def =
  CppSwitch (tag safe $ translateReg reg) (
    map ((CppNum . CppInt) *** prepBranch) cases
  ) (fmap prepBranch def)
    where
      tag :: Bool -> Cpp -> Cpp
      tag True  = cppCTAG
      tag False = cppTAG

      prepBranch :: [BC] -> Cpp
      prepBranch bc = CppSeq $ map (translateBC info) bc

      cppTAG cpp =
        (CppTernary (cpp `cppInstanceOf` "C") (
          CppProj (cppUNBOX cppCON cpp) "tag"
        ) (CppNum (CppInt $ negate 1)))

      cppCTAG :: Cpp -> Cpp
      cppCTAG cpp = CppProj (cppUNBOX cppCON cpp) "tag"

cppCONSTCASE :: CompileInfo -> Reg -> [(Const, [BC])] -> Maybe [BC] -> Cpp
cppCONSTCASE info reg cases def =
  CppCond $ (
    map (unboxedBinOp (cppEq) (translateReg reg) . translateConstant *** prepBranch) cases
  ) ++ (maybe [] ((:[]) . ((,) CppNoop) . prepBranch) def)
    where
      prepBranch :: [BC] -> Cpp
      prepBranch bc = CppSeq $ map (translateBC info) bc

      unboxedBinOp :: (Cpp -> Cpp -> Cpp) -> Cpp -> Cpp -> Cpp
      unboxedBinOp f l r = f (cppUNBOX (unboxedType r) l) r

cppPROJECT :: CompileInfo -> Reg -> Int -> Int -> Cpp
cppPROJECT _ reg loc 0  = CppNoop
cppPROJECT _ reg loc 1  =
  CppAssign (cppLOC loc) (
    CppIndex (CppProj (cppUNBOX cppCON $ translateReg reg) "args")
             (CppNum $ CppInt 0)
  )
cppPROJECT _ reg loc ar =
  CppApp (CppIdent "project") [ cppVM
                              , translateReg reg
                              , CppNum (CppInt loc)
                              , CppNum (CppInt ar)
                              ]

cppOP :: CompileInfo -> Reg -> PrimFn -> [Reg] -> Cpp
cppOP _ reg oper args = CppAssign (translateReg reg) (cppOP' oper)
  where
    cppOP' :: PrimFn -> Cpp
    cppOP' op =
      case op of
        LNoOp -> translateReg (last args)

        (LZExt sty dty) -> cppBOX (cppAType (ATInt dty)) $ cppUNBOX (cppAType (ATInt sty)) $ translateReg (last args)


        (LPlus ty) -> cppBOX (cppAType ty) $ CppBinOp "+" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                          (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LMinus ty) -> cppBOX (cppAType ty) $ CppBinOp "-" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                           (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LTimes ty) -> cppBOX (cppAType ty) $ CppBinOp "*" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                           (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSDiv ty) -> cppBOX (cppAType ty) $ CppBinOp "/" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                          (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSRem ty) -> cppBOX (cppAType ty) $ CppBinOp "%" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                          (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LEq ty) -> cppBOX cppBOOL $ CppBinOp "==" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                   (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSLt ty) -> cppBOX cppBOOL $ CppBinOp "<"  (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                    (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSLe ty) -> cppBOX cppBOOL $ CppBinOp "<=" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                    (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSGt ty) -> cppBOX cppBOOL $ CppBinOp ">"  (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                    (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LSGe ty) -> cppBOX cppBOOL $ CppBinOp ">=" (cppUNBOX (cppAType ty) $ translateReg lhs)
                                                    (cppUNBOX (cppAType ty) $ translateReg rhs)

        (LTrunc ITNative (ITFixed IT8)) -> cppBOX (cppWORD 8) $ CppBinOp "&" (cppUNBOX cppINT $ translateReg arg) (CppRaw "0xFFu")

        (LTrunc (ITFixed IT16) (ITFixed IT8)) -> cppBOX (cppWORD 8) $ CppBinOp "&" (cppUNBOX (cppWORD 16) $ translateReg arg) (CppRaw "0xFFu")

        (LTrunc (ITFixed IT32) (ITFixed IT16)) -> cppBOX (cppWORD 16) $ CppBinOp "&" (cppUNBOX (cppWORD 32) $ translateReg arg) (CppRaw "0xFFFFu")

        (LTrunc (ITFixed IT64) (ITFixed IT32)) -> cppBOX (cppWORD 32) $ CppBinOp "&" (cppUNBOX (cppWORD 64) $ translateReg arg) (CppRaw "0xFFFFFFFFu")

        (LTrunc ITBig (ITFixed IT64)) -> cppBOX (cppWORD 64) $ CppBinOp "&" (cppUNBOX cppBIGINT $ translateReg arg) (CppRaw "0xFFFFFFFFFFFFFFFFu")

        (LTrunc ITBig ITNative) -> cppBOX cppINT $ cppStaticCast (cppINT ++ "::type") (cppUNBOX cppBIGINT $ translateReg arg)

        (LLSHR ty@(ITFixed _)) -> cppOP' (LASHR ty)
        (LLt ty@(ITFixed _))   -> cppOP' (LSLt (ATInt ty))
        (LLe ty@(ITFixed _))   -> cppOP' (LSLe (ATInt ty))
        (LGt ty@(ITFixed _))   -> cppOP' (LSGt (ATInt ty))
        (LGe ty@(ITFixed _))   -> cppOP' (LSGe (ATInt ty))
        (LUDiv ty@(ITFixed _)) -> cppOP' (LSDiv (ATInt ty))

        (LAnd ty) -> cppBOX (cppAType (ATInt ty)) $ CppBinOp "&" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                 (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)

        (LOr ty)   -> cppBOX (cppAType (ATInt ty)) $ CppBinOp " " (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                  (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)

        (LXOr ty)  -> cppBOX (cppAType (ATInt ty)) $ CppBinOp "^" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                  (cppUNBOX (cppAType (ATInt ty)) $ translateReg rhs)

        (LSHL ty)  -> cppBOX (cppAType (ATInt ty)) $ CppBinOp "<<" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                   (cppAsIntegral $ translateReg rhs)

        (LASHR ty) -> cppBOX (cppAType (ATInt ty)) $ CppBinOp ">>" (cppUNBOX (cppAType (ATInt ty)) $ translateReg lhs)
                                                                   (cppAsIntegral $ translateReg rhs)

        (LCompl ty) -> cppBOX (cppAType (ATInt ty)) $ CppPreOp "~" (cppUNBOX (cppAType (ATInt ty)) $ translateReg arg)

        LStrConcat -> cppBOX cppSTRING $ CppBinOp "+" (cppUNBOX cppSTRING $ translateReg lhs)
                                                      (cppUNBOX cppSTRING $ translateReg rhs)

        LStrEq -> cppBOX cppBOOL $ CppBinOp "==" (cppUNBOX cppSTRING $ translateReg lhs)
                                                 (cppUNBOX cppSTRING $ translateReg rhs)

        LStrLt -> cppBOX cppBOOL $ CppBinOp "<"  (cppUNBOX cppSTRING $ translateReg lhs)
                                                 (cppUNBOX cppSTRING $ translateReg rhs)

        LStrLen -> cppBOX cppINT $ cppStaticCast (cppINT ++ "::type") (strLen (cppUNBOX cppSTRING $ translateReg arg)) -- TODO: int size 64?

        (LStrInt ITNative)     -> cppBOX cppINT $ cppCall "stoi" [cppUNBOX cppSTRING $ translateReg arg]
        (LIntStr ITNative)     -> cppBOX cppSTRING $ cppAsString $ translateReg arg
        (LSExt ITNative ITBig) -> cppBOX cppBIGINT $ cppUNBOX cppINT $ translateReg arg
        (LIntStr ITBig)        -> cppBOX cppSTRING $ cppAsString $ translateReg arg
        (LStrInt ITBig)        -> cppBOX cppBIGINT $ cppCall "stoll" [cppUNBOX cppSTRING $ translateReg arg]
        LFloatStr              -> cppBOX cppSTRING $ cppAsString $ translateReg arg
        LStrFloat              -> cppBOX cppFLOAT $ cppCall "stod" [cppUNBOX cppSTRING $ translateReg arg]
        (LIntFloat ITNative)   ->  cppBOX cppFLOAT $ cppUNBOX cppINT $ translateReg arg
        (LFloatInt ITNative)   -> cppBOX cppINT $ cppUNBOX cppFLOAT $ translateReg arg
        (LChInt ITNative)      -> cppBOX cppINT $ cppUNBOX cppCHAR $ translateReg arg
        (LIntCh ITNative)      -> cppBOX cppCHAR $ cppUNBOX cppINT $ translateReg arg

        LFExp   -> cppBOX cppFLOAT $ cppCall "exp" [cppUNBOX cppFLOAT $ translateReg arg]
        LFLog   -> cppBOX cppFLOAT $ cppCall "log" [cppUNBOX cppFLOAT $ translateReg arg]
        LFSin   -> cppBOX cppFLOAT $ cppCall "sin" [cppUNBOX cppFLOAT $ translateReg arg]
        LFCos   -> cppBOX cppFLOAT $ cppCall "cos" [cppUNBOX cppFLOAT $ translateReg arg]
        LFTan   -> cppBOX cppFLOAT $ cppCall "tan" [cppUNBOX cppFLOAT $ translateReg arg]
        LFASin  -> cppBOX cppFLOAT $ cppCall "asin" [cppUNBOX cppFLOAT $ translateReg arg]
        LFACos  -> cppBOX cppFLOAT $ cppCall "acos" [cppUNBOX cppFLOAT $ translateReg arg]
        LFATan  -> cppBOX cppFLOAT $ cppCall "atan" [cppUNBOX cppFLOAT $ translateReg arg]
        LFSqrt  -> cppBOX cppFLOAT $ cppCall "sqrt" [cppUNBOX cppFLOAT $ translateReg arg]
        LFFloor -> cppBOX cppFLOAT $ cppCall "floor" [cppUNBOX cppFLOAT $ translateReg arg]
        LFCeil  -> cppBOX cppFLOAT $ cppCall "ceil" [cppUNBOX cppFLOAT $ translateReg arg]

        LStrCons -> cppBOX cppSTRING $ CppBinOp "+" (cppAsString $ translateReg lhs)
                                                              (cppUNBOX cppSTRING $ translateReg rhs)
        LStrHead -> let str = cppUNBOX cppSTRING $ translateReg arg in
                      CppTernary (cppAnd (translateReg arg) (CppPreOp "!" (cppMeth str "empty" [])))
                                 (cppBOX cppCHAR $ cppCall "utf8_head" [str])
                                 CppNull

        LStrRev     -> cppBOX cppSTRING $ cppCall "reverse" [cppUNBOX cppSTRING $ translateReg arg]

        LStrIndex   -> cppBOX cppCHAR $ cppCall "char32_from_utf8_string" [cppUNBOX cppSTRING $ translateReg lhs,
                                                                                cppAsIntegral $ translateReg rhs]
        LStrTail    -> let str = cppUNBOX cppSTRING $ translateReg arg in
                         CppTernary (cppAnd (translateReg arg) (cppGreaterThan (strLen str) cppOne))
                                    (cppBOX cppSTRING $ cppCall "utf8_tail" [str])
                                    (cppBOX cppSTRING $ CppString "")

        LReadStr    -> cppBOX cppSTRING $ cppCall "freadStr" [cppUNBOX cppManagedPtr $ translateReg arg]

        LSystemInfo -> cppBOX cppSTRING $ cppCall "systemInfo"  [translateReg arg]

        LNullPtr    -> CppNull

        _ -> CppError $ "Not implemented: " ++ show op

        where
          (lhs:rhs:_) = args
          (arg:_) = args
          invokeMeth :: Reg -> String -> [Reg] -> Cpp
          invokeMeth obj meth args =
            CppApp (CppProj (translateReg obj) meth) $ map translateReg args

          strLen :: Cpp -> Cpp
          strLen s = cppMeth s "length" []

cppSTACK :: Cpp
cppSTACK = CppIdent "vm->valstack"

cppCALLSTACK :: Cpp
cppCALLSTACK = CppIdent "vm->callstack"

cppARGSTACK :: Cpp
cppARGSTACK = CppIdent "vm->argstack"

cppSTACKBASE :: Cpp
cppSTACKBASE = CppIdent "vm->valstack_base"

cppSTACKTOP :: Cpp
cppSTACKTOP = CppIdent "vm->valstack_top"

cppBASETYPENAME :: String
cppBASETYPENAME = "IndexType"

cppOLDBASENAME :: String
cppOLDBASENAME = "oldbase"

cppOLDBASE :: Cpp
cppOLDBASE = CppIdent cppOLDBASENAME

cppMYOLDBASENAME :: String
cppMYOLDBASENAME = "myoldbase"

cppMYOLDBASE :: Cpp
cppMYOLDBASE = CppIdent cppMYOLDBASENAME

cppVM :: Cpp
cppVM = CppIdent "vm"

cppFUNCPARMS :: [String]
cppFUNCPARMS = ["shared_ptr<VirtualMachine>& vm", cppBASETYPENAME ++ " " ++ cppOLDBASENAME]

cppRET :: Cpp
cppRET = CppIdent "vm->ret"

cppLOC :: Int -> Cpp
cppLOC 0 = CppIndex cppSTACK cppSTACKBASE
cppLOC n = CppIndex cppSTACK (CppBinOp "+" cppSTACKBASE (CppNum (CppInt n)))

cppTOP :: Int -> Cpp
cppTOP 0 = CppIndex cppSTACK cppSTACKTOP
cppTOP n = CppIndex cppSTACK (CppBinOp "+" cppSTACKTOP (CppNum (CppInt n)))

cppPUSH :: [Cpp] -> Cpp
cppPUSH args = CppApp (CppProj cppCALLSTACK "push") args

cppPUSHARG :: [Cpp] -> Cpp
cppPUSHARG args = CppApp (CppProj cppARGSTACK "push") args

cppPOP :: Cpp
cppPOP = CppBinOp ";" (cppMeth cppCALLSTACK "top" []) (cppMeth cppCALLSTACK "pop" [])

cppPOPARGS :: Cpp
cppPOPARGS = CppBinOp ";" (cppMeth cppARGSTACK "top" []) (cppMeth cppARGSTACK "pop" [])

cppBOX' :: Cpp -> Cpp
cppBOX' obj = cppBOX (unboxedType obj) obj

cppUNBOX :: String -> Cpp -> Cpp
cppUNBOX typ obj = CppApp (CppIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: Cpp -> String
unboxedType e = case e of
                  (CppString _)                       -> cppSTRING
                  (CppNum (CppFloat _))               -> cppFLOAT
                  (CppNum (CppInteger (CppBigInt _))) -> cppBIGINT
                  (CppNum _)                          -> cppINT
                  (CppChar _)                         -> cppCHAR
                  (CppWord (CppWord8 _))              -> cppWORD 8
                  (CppWord (CppWord16 _))             -> cppWORD 16
                  (CppWord (CppWord32 _))             -> cppWORD 32
                  (CppWord (CppWord64 _))             -> cppWORD 64
                  _                                   -> ""

cppBOX :: String -> Cpp -> Cpp
cppBOX typ obj = case typ of
                       "" -> cppCall "box" [obj]
                       _  -> cppCall ("box" ++ "<" ++ typ ++ ">") [obj]

cppAsString :: Cpp -> Cpp
cppAsString obj = cppPtrMeth obj "asString" []

cppAsIntegral :: Cpp -> Cpp
cppAsIntegral obj = cppPtrMeth obj "asIntegral" []

cppINT :: String
cppINT = "Int"

cppBIGINT :: String
cppBIGINT = "BigInt"

cppFLOAT :: String
cppFLOAT = "Float"

cppSTRING :: String
cppSTRING = "String"

cppCHAR :: String
cppCHAR = "Char"

cppWORD :: Int -> String
cppWORD n = PF.printf "Word%d" n

cppManagedPtr :: String
cppManagedPtr = "ManagedPtr"

cppPTR :: String
cppPTR = "Ptr"

cppCON :: String
cppCON = "Con"

cppBOOL :: String
cppBOOL = cppINT

cppAType :: ArithTy -> String
cppAType (ATInt ITNative)       = cppINT
cppAType (ATInt ITBig)          = cppBIGINT
cppAType (ATInt ITChar)         = cppCHAR
cppAType (ATFloat)              = cppFLOAT
cppAType (ATInt (ITFixed IT8))  = cppWORD 8
cppAType (ATInt (ITFixed IT16)) = cppWORD 16
cppAType (ATInt (ITFixed IT32)) = cppWORD 32
cppAType (ATInt (ITFixed IT64)) = cppWORD 64
cppAType (ty)                   = "UNKNOWN TYPE: " ++ show ty

cppCodepoint :: Int -> String
cppCodepoint c = PF.printf "\\U%.8X" c

translateBC :: CompileInfo -> BC -> Cpp
translateBC info bc =
  case bc of
    ASSIGN r1 r2          ->  cppASSIGN info r1 r2
    ASSIGNCONST r c       ->  cppASSIGNCONST info r c
    UPDATE r1 r2          ->  cppASSIGN info r1 r2
    ADDTOP n              ->  cppADDTOP info n
    NULL r                ->  cppNULL info r
    CALL n                ->  cppCALL info n
    TAILCALL n            ->  cppTAILCALL info n
    FOREIGNCALL r _ t n a ->  cppFOREIGN info r n a t
    TOPBASE n             ->  cppTOPBASE info n
    BASETOP n             ->  cppBASETOP info n
    STOREOLD              ->  cppSTOREOLD info
    SLIDE n               ->  cppSLIDE info n
    REBASE                ->  cppREBASE info
    RESERVE n             ->  cppRESERVE info n
    MKCON r _ t rs        ->  cppMKCON info r t rs
    CASE s r c d          ->  cppCASE info s r c d
    CONSTCASE r c d       ->  cppCONSTCASE info r c d
    PROJECT r l a         ->  cppPROJECT info r l a
    OP r o a              ->  cppOP info r o a
    ERROR e               ->  cppERROR info e
    _                     ->  CppRaw $ "//" ++ show bc
