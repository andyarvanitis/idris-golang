{-# LANGUAGE CPP #-}

module IRTS.CodegenCpp (codegenCpp) where

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System hiding (getDataDir)
import IRTS.CodegenCommon
import IRTS.IL.AST
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

data ILTarget = ILExpr deriving Eq

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
      toDecl f = "void " ++ translateName f ++ "(" ++ (intercalate ", " mkILFUNCPARMS) ++ ");\n"

      namespaceBegin :: T.Text
      namespaceBegin = T.pack "namespace idris {\n"

      namespaceEnd :: T.Text
      namespaceEnd = T.pack "} // namespace idris\n"

toCpp info (name, bc) =
  [ ILIdent $ "void " ++ translateName name,
    ILFunction mkILFUNCPARMS (
      ILSeq $ ILAlloc (Just mkILBASETYPENAME) mkILMYOLDBASENAME Nothing
               : ILPreOp "(void)" mkILMYOLDBASE
               : map (translateBC info)bc
    )
  ]

translateReg :: Reg -> ILExpr
translateReg reg =
  case reg of
    RVal -> mkILRET
    Tmp  -> ILRaw "//TMPREG"
    L n  -> mkILLOC n
    T n  -> mkILTOP n

translateConstant :: Const -> ILExpr
translateConstant (I i)                    = ILNum (ILInt i)
translateConstant (Fl f)                   = ILNum (ILFloat f)
translateConstant (Ch c)                   = ILChar (translateChar c)
translateConstant (Str s)                  = ILString $ concatMap translateChar s
translateConstant (AType (ATInt ITNative)) = ILType ILIntTy
translateConstant StrType                  = ILType ILStringTy
translateConstant (AType (ATInt ITBig))    = ILType ILIntegerTy
translateConstant (AType ATFloat)          = ILType ILFloatTy
translateConstant (AType (ATInt ITChar))   = ILType ILCharTy
translateConstant PtrType                  = ILType ILPtrTy
translateConstant Forgot                   = ILType ILForgotTy
translateConstant (BI i)                   = ILNum $ ILInteger (ILBigInt i)
translateConstant (B8 b)                   = ILWord (ILWord8 b)
translateConstant (B16 b)                  = ILWord (ILWord16 b)
translateConstant (B32 b)                  = ILWord (ILWord32 b)
translateConstant (B64 b)                  = ILWord (ILWord64 b)
translateConstant c =
  ILError $ "Unimplemented Constant: " ++ show c

translateChar :: Char -> String
translateChar ch
  | isAscii ch && isAlphaNum ch  = [ch]
  | ch `elem` [' ','_', ',','.'] = [ch]
  | otherwise                    = mkILCodepoint (ord ch)

translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"

mkILASSIGN :: CompileInfo -> Reg -> Reg -> ILExpr
mkILASSIGN _ r1 r2 = ILAssign (translateReg r1) (translateReg r2)

mkILASSIGNCONST :: CompileInfo -> Reg -> Const -> ILExpr
mkILASSIGNCONST _ r c = ILAssign (translateReg r) (mkILBOX' $ translateConstant c)

mkILCALL :: CompileInfo -> Name -> ILExpr
mkILCALL _ n =
  ILApp (
    ILIdent "vm_call"
  ) [mkILVM, ILIdent (translateName n), mkILMYOLDBASE]

mkILTAILCALL :: CompileInfo -> Name -> ILExpr
mkILTAILCALL _ n =
  ILApp (
    ILIdent "vm_tailcall"
  ) [mkILVM, ILIdent (translateName n), mkILOLDBASE]

mkILFOREIGN :: CompileInfo -> Reg -> String -> [(FType, Reg)] -> FType -> ILExpr
mkILFOREIGN _ reg n args ret =
  case n of
    "fileOpen" -> let [(_, name),(_, mode)] = args in
                  ILAssign (translateReg reg)
                            (mkILBOX mkILManagedPtr $ mkILCall "fileOpen" [mkILUNBOX mkILSTRING $ translateReg name,
                                                                        mkILUNBOX mkILSTRING $ translateReg mode])
    "fileClose" -> let [(_, fh)] = args in
                   ILAssign (translateReg reg) (mkILCall "fileClose" [mkILUNBOX mkILManagedPtr $ translateReg fh])

    "fputStr" -> let [(_, fh),(_, str)] = args in
                 ILAssign (translateReg reg) (mkILCall "fputStr" [mkILUNBOX mkILManagedPtr $ translateReg fh,
                                                                      mkILUNBOX mkILSTRING $ translateReg str])
    "fileEOF" -> let [(_, fh)] = args in
                 ILAssign (translateReg reg) (mkILBOX mkILINT $ mkILCall "fileEOF" [mkILUNBOX mkILManagedPtr $ translateReg fh])

    "fileError" -> let [(_, fh)] = args in
                   ILAssign (translateReg reg) (mkILBOX mkILINT $ mkILCall "fileError" [mkILUNBOX mkILManagedPtr $ translateReg fh])

    "isNull" -> let [(_, arg)] = args in
                ILAssign (translateReg reg) (mkILBOX mkILBOOL $ ILBinOp "==" (translateReg arg) ILNull)

    "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                  ILAssign (translateReg reg) (ILBinOp "==" (translateReg lhs) (translateReg rhs))

    "getenv" -> let [(_, arg)] = args in
                ILAssign (translateReg reg) (mkILBOX mkILSTRING $ mkILCall "getenv" [mkILMeth (mkILUNBOX mkILSTRING $ translateReg arg) "c_str" []])

    _ -> ILAssign (translateReg reg) (let callexpr = ILFFI n (map generateWrapper args) in
                                       case ret of
                                         FUnit -> ILBinOp "," ILNull callexpr
                                         _     -> mkILBOX (T.unpack . compileIL $ foreignToBoxed ret) $ callexpr)
    where
      generateWrapper :: (FType, Reg) -> ILExpr
      generateWrapper (ty, reg) =
        case ty of
          FFunction aty rty ->
            ILApp (ILIdent $ "LAMBDA_WRAPPER") [translateReg reg, cType aty, cType rty]
          FFunctionIO -> error "FFunctionIO not supported yet"
          _ -> mkILUNBOX (T.unpack . compileIL $ foreignToBoxed ty) $ translateReg reg

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
      foreignToBoxed (FArith (ATInt ITNative))       = ILIdent mkILINT
      foreignToBoxed (FArith (ATInt ITChar))         = ILIdent mkILCHAR
      foreignToBoxed (FArith (ATInt ITBig))          = ILIdent mkILBIGINT
      foreignToBoxed (FArith (ATInt (ITFixed IT8)))  = ILIdent (mkILWORD 8)
      foreignToBoxed (FArith (ATInt (ITFixed IT16))) = ILIdent (mkILWORD 16)
      foreignToBoxed (FArith (ATInt (ITFixed IT32))) = ILIdent (mkILWORD 32)
      foreignToBoxed (FArith (ATInt (ITFixed IT64))) = ILIdent (mkILWORD 64)
      foreignToBoxed FString = ILIdent mkILSTRING
      -- foreignToBoxed FUnit = ILIdent "void"
      foreignToBoxed FPtr = ILIdent mkILPTR
      foreignToBoxed FManagedPtr = ILIdent mkILManagedPtr
      foreignToBoxed (FArith ATFloat) = ILIdent mkILFLOAT
      foreignToBoxed FAny = ILIdent mkILPTR
      -- foreignToBoxed (FFunction a b) = ILList [cType a, cType b]

mkILREBASE :: CompileInfo -> ILExpr
mkILREBASE _ = ILAssign mkILSTACKBASE mkILOLDBASE

mkILSTOREOLD :: CompileInfo ->ILExpr
mkILSTOREOLD _ = ILAssign mkILMYOLDBASE mkILSTACKBASE

mkILADDTOP :: CompileInfo -> Int -> ILExpr
mkILADDTOP info n = case n of
                     0 -> ILNoop
                     _ -> ILBinOp "+=" mkILSTACKTOP (ILNum (ILInt n))

mkILTOPBASE :: CompileInfo -> Int -> ILExpr
mkILTOPBASE _ 0  = ILAssign mkILSTACKTOP mkILSTACKBASE
mkILTOPBASE _ n  = ILAssign mkILSTACKTOP (ILBinOp "+" mkILSTACKBASE (ILNum (ILInt n)))

mkILBASETOP :: CompileInfo -> Int -> ILExpr
mkILBASETOP _ 0 = ILAssign mkILSTACKBASE mkILSTACKTOP
mkILBASETOP _ n = ILAssign mkILSTACKBASE (ILBinOp "+" mkILSTACKTOP (ILNum (ILInt n)))

mkILNULL :: CompileInfo -> Reg -> ILExpr
mkILNULL _ r = ILAssign (translateReg r) ILNull

mkILERROR :: CompileInfo -> String -> ILExpr
mkILERROR _ = ILError

mkILSLIDE :: CompileInfo -> Int -> ILExpr
mkILSLIDE _ n = ILApp (ILIdent "slide") [mkILVM, ILNum (ILInt n)]

mkILRESERVE :: CompileInfo -> Int -> ILExpr
mkILRESERVE _ n = mkILCall "reserve" [mkILVM, ILBinOp "+" mkILSTACKTOP (ILNum $ ILInt n)]

mkILMKCON :: CompileInfo -> Reg -> Int -> [Reg] -> ILExpr
mkILMKCON info r t rs =
  ILAssign (translateReg r) (
    mkILBOX mkILCON $ ILList $ ILNum (ILInt t) : args rs
  )
    where
      args [] = []
      args xs = [ILList (map translateReg xs)]

mkILCASE :: CompileInfo -> Bool -> Reg -> [(Int, [BC])] -> Maybe [BC] -> ILExpr
mkILCASE info safe reg cases def =
  ILSwitch (tag safe $ translateReg reg) (
    map ((ILNum . ILInt) *** prepBranch) cases
  ) (fmap prepBranch def)
    where
      tag :: Bool -> ILExpr -> ILExpr
      tag True  = mkILCTAG
      tag False = mkILTAG

      prepBranch :: [BC] -> ILExpr
      prepBranch bc = ILSeq $ map (translateBC info) bc

      mkILTAG expr =
        (ILTernary (expr `mkILInstanceOf` "Con::typeId") (
          ILProj (mkILUNBOX mkILCON expr) "tag"
        ) (ILNum (ILInt $ negate 1)))

      mkILCTAG :: ILExpr -> ILExpr
      mkILCTAG expr = ILProj (mkILUNBOX mkILCON expr) "tag"

mkILCONSTCASE :: CompileInfo -> Reg -> [(Const, [BC])] -> Maybe [BC] -> ILExpr
mkILCONSTCASE info reg cases def =
  ILCond $ (
    map (unboxedBinOp (mkILEq) (translateReg reg) . translateConstant *** prepBranch) cases
  ) ++ (maybe [] ((:[]) . ((,) ILNoop) . prepBranch) def)
    where
      prepBranch :: [BC] -> ILExpr
      prepBranch bc = ILSeq $ map (translateBC info) bc

      unboxedBinOp :: (ILExpr -> ILExpr -> ILExpr) -> ILExpr -> ILExpr -> ILExpr
      unboxedBinOp f l r = f (mkILUNBOX (unboxedType r) l) r

mkILPROJECT :: CompileInfo -> Reg -> Int -> Int -> ILExpr
mkILPROJECT _ reg loc 0  = ILNoop
mkILPROJECT _ reg loc ar =
  ILApp (ILIdent "project") [ mkILVM
                              , translateReg reg
                              , ILNum (ILInt loc)
                              , ILNum (ILInt ar)
                              ]

mkILOP :: CompileInfo -> Reg -> PrimFn -> [Reg] -> ILExpr
mkILOP _ reg oper args = ILAssign (translateReg reg) (mkILOP' oper)
  where
    mkILOP' :: PrimFn -> ILExpr
    mkILOP' op =
      case op of
        LNoOp -> translateReg (last args)

        (LZExt sty dty) -> mkILBOX (mkILAType (ATInt dty)) $ mkILUNBOX (mkILAType (ATInt sty)) $ translateReg (last args)


        (LPlus ty) -> mkILBOX (mkILAType ty) $ ILBinOp "+" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                          (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LMinus ty) -> mkILBOX (mkILAType ty) $ ILBinOp "-" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                           (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LTimes ty) -> mkILBOX (mkILAType ty) $ ILBinOp "*" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                           (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSDiv ty) -> mkILBOX (mkILAType ty) $ ILBinOp "/" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                          (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSRem ty) -> mkILBOX (mkILAType ty) $ ILBinOp "%" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                          (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LEq ty) -> mkILBOX mkILBOOL $ ILBinOp "==" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                   (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSLt ty) -> mkILBOX mkILBOOL $ ILBinOp "<"  (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                    (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSLe ty) -> mkILBOX mkILBOOL $ ILBinOp "<=" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                    (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSGt ty) -> mkILBOX mkILBOOL $ ILBinOp ">"  (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                    (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LSGe ty) -> mkILBOX mkILBOOL $ ILBinOp ">=" (mkILUNBOX (mkILAType ty) $ translateReg lhs)
                                                    (mkILUNBOX (mkILAType ty) $ translateReg rhs)

        (LTrunc ITNative (ITFixed IT8)) -> mkILBOX (mkILWORD 8) $ ILBinOp "&" (mkILUNBOX mkILINT $ translateReg arg) (ILRaw "0xFFu")

        (LTrunc (ITFixed IT16) (ITFixed IT8)) -> mkILBOX (mkILWORD 8) $ ILBinOp "&" (mkILUNBOX (mkILWORD 16) $ translateReg arg) (ILRaw "0xFFu")

        (LTrunc (ITFixed IT32) (ITFixed IT16)) -> mkILBOX (mkILWORD 16) $ ILBinOp "&" (mkILUNBOX (mkILWORD 32) $ translateReg arg) (ILRaw "0xFFFFu")

        (LTrunc (ITFixed IT64) (ITFixed IT32)) -> mkILBOX (mkILWORD 32) $ ILBinOp "&" (mkILUNBOX (mkILWORD 64) $ translateReg arg) (ILRaw "0xFFFFFFFFu")

        (LTrunc ITBig (ITFixed IT64)) -> mkILBOX (mkILWORD 64) $ ILBinOp "&" (mkILUNBOX mkILBIGINT $ translateReg arg) (ILRaw "0xFFFFFFFFFFFFFFFFu")

        (LTrunc ITBig ITNative) -> mkILBOX mkILINT $ mkILStaticCast (mkILINT ++ "::type") (mkILUNBOX mkILBIGINT $ translateReg arg)

        (LLSHR ty@(ITFixed _)) -> mkILOP' (LASHR ty)
        (LLt ty@(ITFixed _))   -> mkILOP' (LSLt (ATInt ty))
        (LLe ty@(ITFixed _))   -> mkILOP' (LSLe (ATInt ty))
        (LGt ty@(ITFixed _))   -> mkILOP' (LSGt (ATInt ty))
        (LGe ty@(ITFixed _))   -> mkILOP' (LSGe (ATInt ty))
        (LUDiv ty@(ITFixed _)) -> mkILOP' (LSDiv (ATInt ty))

        (LAnd ty) -> mkILBOX (mkILAType (ATInt ty)) $ ILBinOp "&" (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg lhs)
                                                                 (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg rhs)

        (LOr ty)   -> mkILBOX (mkILAType (ATInt ty)) $ ILBinOp " " (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg lhs)
                                                                  (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg rhs)

        (LXOr ty)  -> mkILBOX (mkILAType (ATInt ty)) $ ILBinOp "^" (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg lhs)
                                                                  (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg rhs)

        (LSHL ty)  -> mkILBOX (mkILAType (ATInt ty)) $ ILBinOp "<<" (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg lhs)
                                                                   (mkILAsIntegral $ translateReg rhs)

        (LASHR ty) -> mkILBOX (mkILAType (ATInt ty)) $ ILBinOp ">>" (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg lhs)
                                                                   (mkILAsIntegral $ translateReg rhs)

        (LCompl ty) -> mkILBOX (mkILAType (ATInt ty)) $ ILPreOp "~" (mkILUNBOX (mkILAType (ATInt ty)) $ translateReg arg)

        LStrConcat -> mkILBOX mkILSTRING $ ILBinOp "+" (mkILUNBOX mkILSTRING $ translateReg lhs)
                                                      (mkILUNBOX mkILSTRING $ translateReg rhs)

        LStrEq -> mkILBOX mkILBOOL $ ILBinOp "==" (mkILUNBOX mkILSTRING $ translateReg lhs)
                                                 (mkILUNBOX mkILSTRING $ translateReg rhs)

        LStrLt -> mkILBOX mkILBOOL $ ILBinOp "<"  (mkILUNBOX mkILSTRING $ translateReg lhs)
                                                 (mkILUNBOX mkILSTRING $ translateReg rhs)

        LStrLen -> mkILBOX mkILINT $ mkILStaticCast (mkILINT ++ "::type") (strLen (mkILUNBOX mkILSTRING $ translateReg arg)) -- TODO: int size 64?

        (LStrInt ITNative)     -> mkILBOX mkILINT $ mkILCall "stoi" [mkILUNBOX mkILSTRING $ translateReg arg]
        (LIntStr ITNative)     -> mkILBOX mkILSTRING $ mkILAsString $ translateReg arg
        (LSExt ITNative ITBig) -> mkILBOX mkILBIGINT $ mkILUNBOX mkILINT $ translateReg arg
        (LIntStr ITBig)        -> mkILBOX mkILSTRING $ mkILAsString $ translateReg arg
        (LStrInt ITBig)        -> mkILBOX mkILBIGINT $ mkILCall "stoll" [mkILUNBOX mkILSTRING $ translateReg arg]
        LFloatStr              -> mkILBOX mkILSTRING $ mkILAsString $ translateReg arg
        LStrFloat              -> mkILBOX mkILFLOAT $ mkILCall "stod" [mkILUNBOX mkILSTRING $ translateReg arg]
        (LIntFloat ITNative)   ->  mkILBOX mkILFLOAT $ mkILUNBOX mkILINT $ translateReg arg
        (LFloatInt ITNative)   -> mkILBOX mkILINT $ mkILUNBOX mkILFLOAT $ translateReg arg
        (LChInt ITNative)      -> mkILBOX mkILINT $ mkILUNBOX mkILCHAR $ translateReg arg
        (LIntCh ITNative)      -> mkILBOX mkILCHAR $ mkILUNBOX mkILINT $ translateReg arg

        LFExp   -> mkILBOX mkILFLOAT $ mkILCall "exp" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFLog   -> mkILBOX mkILFLOAT $ mkILCall "log" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFSin   -> mkILBOX mkILFLOAT $ mkILCall "sin" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFCos   -> mkILBOX mkILFLOAT $ mkILCall "cos" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFTan   -> mkILBOX mkILFLOAT $ mkILCall "tan" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFASin  -> mkILBOX mkILFLOAT $ mkILCall "asin" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFACos  -> mkILBOX mkILFLOAT $ mkILCall "acos" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFATan  -> mkILBOX mkILFLOAT $ mkILCall "atan" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFSqrt  -> mkILBOX mkILFLOAT $ mkILCall "sqrt" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFFloor -> mkILBOX mkILFLOAT $ mkILCall "floor" [mkILUNBOX mkILFLOAT $ translateReg arg]
        LFCeil  -> mkILBOX mkILFLOAT $ mkILCall "ceil" [mkILUNBOX mkILFLOAT $ translateReg arg]

        LStrCons -> mkILBOX mkILSTRING $ ILBinOp "+" (mkILAsString $ translateReg lhs)
                                                              (mkILUNBOX mkILSTRING $ translateReg rhs)
        LStrHead -> let str = mkILUNBOX mkILSTRING $ translateReg arg in
                      ILTernary (mkILAnd (translateReg arg) (ILPreOp "!" (mkILMeth str "empty" [])))
                                 (mkILBOX mkILCHAR $ mkILCall "utf8_head" [str])
                                 ILNull

        LStrRev     -> mkILBOX mkILSTRING $ mkILCall "reverse" [mkILUNBOX mkILSTRING $ translateReg arg]

        LStrIndex   -> mkILBOX mkILCHAR $ mkILCall "char32_from_utf8_string" [mkILUNBOX mkILSTRING $ translateReg lhs,
                                                                                mkILAsIntegral $ translateReg rhs]
        LStrTail    -> let str = mkILUNBOX mkILSTRING $ translateReg arg in
                         ILTernary (mkILAnd (translateReg arg) (mkILGreaterThan (strLen str) mkILOne))
                                    (mkILBOX mkILSTRING $ mkILCall "utf8_tail" [str])
                                    (mkILBOX mkILSTRING $ ILString "")

        LReadStr    -> mkILBOX mkILSTRING $ mkILCall "freadStr" [mkILUNBOX mkILManagedPtr $ translateReg arg]

        LSystemInfo -> mkILBOX mkILSTRING $ mkILCall "systemInfo"  [translateReg arg]

        LNullPtr    -> ILNull

        _ -> ILError $ "Not implemented: " ++ show op

        where
          (lhs:rhs:_) = args
          (arg:_) = args
          invokeMeth :: Reg -> String -> [Reg] -> ILExpr
          invokeMeth obj meth args =
            ILApp (ILProj (translateReg obj) meth) $ map translateReg args

          strLen :: ILExpr -> ILExpr
          strLen s = mkILMeth s "length" []

mkILSTACK :: ILExpr
mkILSTACK = ILIdent "vm->valstack"

mkILCALLSTACK :: ILExpr
mkILCALLSTACK = ILIdent "vm->callstack"

mkILARGSTACK :: ILExpr
mkILARGSTACK = ILIdent "vm->argstack"

mkILSTACKBASE :: ILExpr
mkILSTACKBASE = ILIdent "vm->valstack_base"

mkILSTACKTOP :: ILExpr
mkILSTACKTOP = ILIdent "vm->valstack_top"

mkILBASETYPENAME :: String
mkILBASETYPENAME = "IndexType"

mkILOLDBASENAME :: String
mkILOLDBASENAME = "oldbase"

mkILOLDBASE :: ILExpr
mkILOLDBASE = ILIdent mkILOLDBASENAME

mkILMYOLDBASENAME :: String
mkILMYOLDBASENAME = "myoldbase"

mkILMYOLDBASE :: ILExpr
mkILMYOLDBASE = ILIdent mkILMYOLDBASENAME

mkILVM :: ILExpr
mkILVM = ILIdent "vm"

mkILFUNCPARMS :: [String]
mkILFUNCPARMS = ["shared_ptr<VirtualMachine>& vm", mkILBASETYPENAME ++ " " ++ mkILOLDBASENAME]

mkILRET :: ILExpr
mkILRET = ILIdent "vm->ret"

mkILLOC :: Int -> ILExpr
mkILLOC 0 = ILIndex mkILSTACK mkILSTACKBASE
mkILLOC n = ILIndex mkILSTACK (ILBinOp "+" mkILSTACKBASE (ILNum (ILInt n)))

mkILTOP :: Int -> ILExpr
mkILTOP 0 = ILIndex mkILSTACK mkILSTACKTOP
mkILTOP n = ILIndex mkILSTACK (ILBinOp "+" mkILSTACKTOP (ILNum (ILInt n)))

mkILPUSH :: [ILExpr] -> ILExpr
mkILPUSH args = ILApp (ILProj mkILCALLSTACK "push") args

mkILPUSHARG :: [ILExpr] -> ILExpr
mkILPUSHARG args = ILApp (ILProj mkILARGSTACK "push") args

mkILPOP :: ILExpr
mkILPOP = ILBinOp ";" (mkILMeth mkILCALLSTACK "top" []) (mkILMeth mkILCALLSTACK "pop" [])

mkILPOPARGS :: ILExpr
mkILPOPARGS = ILBinOp ";" (mkILMeth mkILARGSTACK "top" []) (mkILMeth mkILARGSTACK "pop" [])

mkILBOX' :: ILExpr -> ILExpr
mkILBOX' obj = mkILBOX (unboxedType obj) obj

mkILUNBOX :: String -> ILExpr -> ILExpr
mkILUNBOX typ obj = ILApp (ILIdent $ "unbox" ++ "<" ++ typ ++ ">") [obj]

unboxedType :: ILExpr -> String
unboxedType e = case e of
                  (ILString _)                       -> mkILSTRING
                  (ILNum (ILFloat _))               -> mkILFLOAT
                  (ILNum (ILInteger (ILBigInt _))) -> mkILBIGINT
                  (ILNum _)                          -> mkILINT
                  (ILChar _)                         -> mkILCHAR
                  (ILWord (ILWord8 _))              -> mkILWORD 8
                  (ILWord (ILWord16 _))             -> mkILWORD 16
                  (ILWord (ILWord32 _))             -> mkILWORD 32
                  (ILWord (ILWord64 _))             -> mkILWORD 64
                  _                                   -> ""

mkILBOX :: String -> ILExpr -> ILExpr
mkILBOX typ obj = case typ of
                       "" -> mkILCall "box" [obj]
                       _  -> mkILCall ("box" ++ "<" ++ typ ++ ">") [obj]

mkILAsString :: ILExpr -> ILExpr
mkILAsString obj = mkILPtrMeth obj "asString" []

mkILAsIntegral :: ILExpr -> ILExpr
mkILAsIntegral obj = mkILPtrMeth obj "asIntegral" []

mkILINT :: String
mkILINT = "Int"

mkILBIGINT :: String
mkILBIGINT = "BigInt"

mkILFLOAT :: String
mkILFLOAT = "Float"

mkILSTRING :: String
mkILSTRING = "String"

mkILCHAR :: String
mkILCHAR = "Char"

mkILWORD :: Int -> String
mkILWORD n = PF.printf "Word%d" n

mkILManagedPtr :: String
mkILManagedPtr = "ManagedPtr"

mkILPTR :: String
mkILPTR = "Ptr"

mkILCON :: String
mkILCON = "Con"

mkILBOOL :: String
mkILBOOL = mkILINT

mkILAType :: ArithTy -> String
mkILAType (ATInt ITNative)       = mkILINT
mkILAType (ATInt ITBig)          = mkILBIGINT
mkILAType (ATInt ITChar)         = mkILCHAR
mkILAType (ATFloat)              = mkILFLOAT
mkILAType (ATInt (ITFixed IT8))  = mkILWORD 8
mkILAType (ATInt (ITFixed IT16)) = mkILWORD 16
mkILAType (ATInt (ITFixed IT32)) = mkILWORD 32
mkILAType (ATInt (ITFixed IT64)) = mkILWORD 64
mkILAType (ty)                   = "UNKNOWN TYPE: " ++ show ty

mkILCodepoint :: Int -> String
mkILCodepoint c = PF.printf "\\U%.8X" c

translateBC :: CompileInfo -> BC -> ILExpr
translateBC info bc =
  case bc of
    ASSIGN r1 r2          ->  mkILASSIGN info r1 r2
    ASSIGNCONST r c       ->  mkILASSIGNCONST info r c
    UPDATE r1 r2          ->  mkILASSIGN info r1 r2
    ADDTOP n              ->  mkILADDTOP info n
    NULL r                ->  mkILNULL info r
    CALL n                ->  mkILCALL info n
    TAILCALL n            ->  mkILTAILCALL info n
    FOREIGNCALL r _ t n a ->  mkILFOREIGN info r n a t
    TOPBASE n             ->  mkILTOPBASE info n
    BASETOP n             ->  mkILBASETOP info n
    STOREOLD              ->  mkILSTOREOLD info
    SLIDE n               ->  mkILSLIDE info n
    REBASE                ->  mkILREBASE info
    RESERVE n             ->  mkILRESERVE info n
    MKCON r _ t rs        ->  mkILMKCON info r t rs
    CASE s r c d          ->  mkILCASE info s r c d
    CONSTCASE r c d       ->  mkILCONSTCASE info r c d
    PROJECT r l a         ->  mkILPROJECT info r l a
    OP r o a              ->  mkILOP info r o a
    ERROR e               ->  mkILERROR info e
    _                     ->  ILRaw $ "//" ++ show bc
