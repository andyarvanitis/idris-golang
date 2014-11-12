{-# LANGUAGE OverloadedStrings #-}

module IRTS.CodegenGo (codegenGo) where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
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

data CompileGo = CompileGo Bool -- TODO: just a placeholder

codegenGo :: CodeGenerator
codegenGo ci =
  codegenGo_all (simpleDecls ci)
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

codegenGo_all ::
     [(Name, SDecl)] -> -- declarations/definitions
     OutputType ->      -- output type
     FilePath ->        -- output file name
     [FilePath] ->      -- include files
     String ->          -- extra object files`as
     String ->          -- libraries
     String ->          -- extra compiler flags
     DbgLevel ->        -- debug level
     IO ()

codegenGo_all definitions outputType filename includes objs libs flags dbg = do
  let info = CompileGo True
  let bytecode = map toBC definitions
  let go = concatMap (toGo info) bytecode
  path <- getDataDir
  let goout = (  T.pack "package main\n\n"
                  `T.append` mkImport "reflect"
                  `T.append` mkImport "os"
                  `T.append` mkImport "strconv"
                  `T.append` mkImport "unicode/utf8"
                  `T.append` mkImport "fmt"
                  `T.append` mkImport "math"
                  `T.append` mkQualifiedImport "math/big"
                  `T.append` mkImport "idris_runtime"
                  `T.append` "\n"
                  `T.append` T.concat (map (compile info) go)
                  `T.append` mkIgnoreUnusedImports
                  `T.append` "\n"
                  `T.append` mkMain
                  `T.append` "\n"
               )
  case outputType of
    Raw -> TIO.writeFile filename goout
    _ -> do (tmpn, tmph) <- tempfile
            hPutStr tmph (T.unpack goout)
            hFlush tmph
            hClose tmph
            let cc =
                     "GOPATH=${GOPATH}:" ++ path ++ "; " ++
                     "mv " ++ tmpn ++ " " ++ path ++ "/src/main/main.go; " ++
                     "go build main; " ++
                     "mv main " ++ filename
            exit <- system cc
            when (exit /= ExitSuccess) $
              putStrLn ("FAILURE: " ++ cc)
    where
      mkImport :: String -> T.Text
      mkImport pkg = T.pack $ PF.printf "import . \"%s\"\n" pkg

      mkQualifiedImport :: String -> T.Text
      mkQualifiedImport pkg = T.pack $ PF.printf "import \"%s\"\n" pkg

      mkIgnoreUnusedImports = T.pack $ foldr (++) "\n" (map ("\nconst _ = " ++) consts)
        where consts = ["IntSize", "UTFMax", "Pi", "MaxBase", "DevNull"]

      mkMain = T.pack $ "func main () {\n" ++
                        "  vm := VirtualMachine{}\n" ++
                        "  Call(&vm, _idris__123_runMain0_125_, 0)\n" ++
                        "}\n"
toGo info (name, bc) =
  [ ASTIdent $ "func " ++ translateName name,
    ASTFunction fnParams (
      ASTSeq $ ASTAlloc (Just baseType) myoldbase Nothing
               : ASTAssign (ASTIdent "_") mkMyOldbase
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
instance CompileInfo CompileGo where
-------------------------------------------------------------------------------
  mkAssign _ r1 r2 = ASTAssign (translateReg r1) (translateReg r2)

  mkAssignConst _ r c =
    ASTAssign (translateReg r) (case value of
                                 ASTNum (ASTInteger (ASTBigInt i)) -> bigValue i
                                 _                                 -> mkCast (translatedType value) value)
      where value = translateConstant c
            bigValue i
              | i > (toInteger (maxBound::Int)) || i < (toInteger (minBound::Int)) = mkBigIntStr (show i)
              | otherwise = mkBigInt value

  mkAddTop info n = case n of
                      0 -> ASTNoop
                      _ -> ASTBinOp "+=" mkStacktop (ASTNum (ASTInt n))

  mkNullAssign _ r = ASTAssign (translateReg r) mkNull

  mkVmCall _ n = mkCall "Call" [mkVm, ASTIdent (translateName n), mkMyOldbase]

  mkVmTailCall _ n = mkCall "TailCall" [mkVm, ASTIdent (translateName n), mkOldbase]

  mkForeign info reg n args ret =
    case n of
      "putStr" -> let [(_, str)] = args in
                   ASTAssign (translateReg reg) 
                             (ASTBinOp ";" mkNull (mkCall "Print" [mkUnbox stringTy $ translateReg str]))

      "fileOpen" -> let [(_, name),(_, mode)] = args in
                    ASTAssign (translateReg reg)
                              (mkCall "FileOpen" [mkUnbox stringTy $ translateReg name,
                                                  mkUnbox stringTy $ translateReg mode])
      "fileClose" -> let [(_, fh)] = args in
                     ASTAssign (translateReg reg) (mkMeth (mkUnbox fileTy $ translateReg fh) "Close" [])

      "fputStr" -> let [(_, fh),(_, str)] = args in
                   ASTAssign (translateReg reg) 
                             (mkMeth (mkUnbox fileTy $ translateReg fh) 
                                     "WriteString" 
                                     [mkUnbox stringTy $ translateReg str])

      "fileEOF" -> let [(_, fh)] = args in error "fileEOF not supported yet"
      "fileError" -> let [(_, fh)] = args in error "fileError not supported yet"

      "isNull" -> let [(_, arg)] = args in
                  ASTAssign (translateReg reg) (mkBoolToInt $ mkEq (translateReg arg) mkNull)

      "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                    ASTAssign (translateReg reg) (mkBoolToInt $ mkEq (translateReg lhs) (translateReg rhs))

      "getenv" -> let [(_, arg)] = args in
                  ASTAssign (translateReg reg) (mkCall "Getenv" [mkUnbox stringTy $ translateReg arg])

      _ -> ASTAssign (translateReg reg) (let callexpr = ASTFFI n (map generateWrapper args) in
                                         case ret of
                                           FUnit -> ASTBinOp ";" mkNull callexpr
                                           _     -> callexpr)
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
        cType FManagedPtr = ASTIdent "*interface{}" -- TODO: placeholder
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

  mkSlide _ n = mkCall "Slide" [mkVm, ASTNum (ASTInt n)]

  mkRebase _ = ASTAssign mkStackbase mkOldbase

  mkReserve _ n = mkCall "Reserve" [mkVm, mkAdd mkStacktop (ASTNum $ ASTInt n)]

  mkMakeCon info r t rs = 
    ASTAssign (translateReg r) (mkCall "MakeCon" [ASTList $ ASTNum (ASTInt t) : args rs])
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
        unboxedBinOp f l r = f (mkUnbox (translatedType r) l) r

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

        mkTag expr = mkCall "GetTag" [expr]

        mkCTag :: ASTNode -> ASTNode
        mkCTag expr = mkCall "GetTag" [expr]

  mkProject _ reg loc 0  = ASTNoop
  mkProject _ reg loc ar = mkCall "Project" [mkVm, translateReg reg, ASTNum (ASTInt loc), ASTNum (ASTInt ar)]


  mkOp _ reg (LEq (ATInt ITBig)) (lhs:rhs:_) = mkBigBinOp  reg lhs rhs "Add"

  mkOp _ reg (LPlus (ATInt ITBig)) (lhs:rhs:_)
     | lhs == reg || rhs == reg = mkBigBinOp  reg lhs rhs "Add"
     | otherwise                = mkBigBinOp' reg lhs rhs "Add"

  -- mkOp _ reg (LMinus (ATInt ITBig)) (lhs:rhs:_) = error "BIG LMinus"
  --
  -- mkOp _ reg (LTimes (ATInt ITBig)) (lhs:rhs:_) = error "BIG LTimes"
  --
  -- mkOp _ reg (LSDiv (ATInt ITBig)) (lhs:rhs:_) = error "BIG LSDiv"
  --
  -- mkOp _ reg (LSRem (ATInt ITBig)) (lhs:rhs:_) = error "BIG LSRem"

  mkOp _ reg oper args = ASTAssign (translateReg reg) (mkOp' oper)
    where
      mkOp' :: PrimFn -> ASTNode
      mkOp' op =
        case op of
          LNoOp -> translateReg (last args)

          (LZExt sty dty) -> boxedIntegral dty $ unboxedIntegral sty (last args)
          (LSExt sty dty) -> mkOp' (LZExt sty dty)

          (LPlus ty)  -> mkAdd      (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LMinus ty) -> mkSubtract (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LTimes ty) -> mkMultiply (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSDiv ty)  -> mkDivide   (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSRem ty)  -> mkModulo   (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LEq ty)  -> mkBoolToInt $ mkEq            (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSLt ty) -> mkBoolToInt $ mkLessThan      (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSLe ty) -> mkBoolToInt $ mkLessThanEq    (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSGt ty) -> mkBoolToInt $ mkGreaterThan   (unboxedNum ty lhs) (unboxedNum ty rhs)
          (LSGe ty) -> mkBoolToInt $ mkGreaterThanEq (unboxedNum ty lhs) (unboxedNum ty rhs)

          (LTrunc ITNative (ITFixed IT8))        -> mkTrunc intTy        8  "0xFF"
          (LTrunc (ITFixed IT16) (ITFixed IT8))  -> mkTrunc (wordTy 16)  8  "0xFF"
          (LTrunc (ITFixed IT32) (ITFixed IT16)) -> mkTrunc (wordTy 32) 16  "0xFFFF"
          (LTrunc (ITFixed IT64) (ITFixed IT32)) -> mkTrunc (wordTy 64) 32  "0xFFFFFFFF"
          (LTrunc ITBig (ITFixed IT64))          -> mkTrunc bigIntTy    64  "0xFFFFFFFFFFFFFFFF"

          (LTrunc ITBig ITNative) -> mkCast (intTy) (mkUnbox bigIntTy $ translateReg arg)

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

          LStrConcat -> mkAdd (unboxedString lhs) (unboxedString rhs)
          LStrEq     -> mkBoolToInt $ mkEq (unboxedString lhs) (unboxedString rhs)
          LStrLt     -> mkBoolToInt $ mkLessThan (unboxedString lhs) (unboxedString rhs)
          LStrLen    -> mkCast (intTy) (strLen (mkUnbox stringTy $ translateReg arg)) -- TODO: int size 64?

          (LStrInt ITNative)     -> mkCall "Atoi" [unboxedString arg]
          (LIntStr ITNative)     -> mkAsString $ translateReg arg
          (LIntStr ITBig)        -> mkAsString $ translateReg arg
          (LStrInt ITBig)        -> ASTBinOp "," (ASTIdent "_")
                                                 (mkCall "ParseInt" [unboxedString arg,
                                                                     mkInt 10,
                                                                     mkInt 64]) -- TODO: use bignum lib
          LFloatStr              -> mkAsString $ translateReg arg
          LStrFloat              -> ASTBinOp "," (ASTIdent "_")
                                                 (mkCall "ParseFloat" [unboxedString arg,
                                                                       ASTNum (ASTInt 64)]) -- TODO: use bignum lib

          (LIntFloat ITNative)   -> mkCast floatTy (mkUnbox intTy $ translateReg arg)
          (LFloatInt ITNative)   -> mkCast intTy   (mkUnbox floatTy $ translateReg arg)
          (LChInt ITNative)      -> mkCast intTy   (mkUnbox charTy $ translateReg arg)
          (LIntCh ITNative)      -> mkCast charTy  (mkUnbox intTy $ translateReg arg)

          LFExp   -> floatfn "Exp"   arg
          LFLog   -> floatfn "Log"   arg
          LFSin   -> floatfn "Sin"   arg
          LFCos   -> floatfn "Cos"   arg
          LFTan   -> floatfn "Tan"   arg
          LFASin  -> floatfn "Asin"  arg
          LFACos  -> floatfn "Acos"  arg
          LFATan  -> floatfn "Atan"  arg
          LFSqrt  -> floatfn "Sqrt"  arg
          LFFloor -> floatfn "Floor" arg
          LFCeil  -> floatfn "Ceil"  arg

          LStrCons -> mkCall "Sprintf" [ASTString "%c%s", mkUnbox charTy $ translateReg lhs, unboxedString rhs]

          LStrHead -> ASTIndex (unboxedString arg) mkZero

          LStrRev   -> mkCall "reverse" [mkUnbox stringTy $ translateReg arg]

          LStrIndex -> ASTIndex (unboxedString arg) (mkAsIntegral $ translateReg rhs)

          LStrTail  -> ASTIndex (unboxedString arg) (ASTRaw "1:")

          LReadStr    -> mkCall "FileReadLine" [mkUnbox fileTy $ translateReg arg]
          LSystemInfo -> ASTString "golang backend (stub version info)"
          LNullPtr    -> mkNull

          _ -> ASTError $ "Not implemented: " ++ show op

          where
            (lhs:rhs:_) = args
            (arg:_) = args

            mkTrunc src dst mask = mkBitAnd (mkUnbox src $ translateReg arg) (ASTRaw mask)

            strLen s = mkCall "len" [s]

            boxedIntegral ty expr = mkCall (arithTy (ATInt ty)) [expr]

            unboxedString reg = mkUnbox stringTy (translateReg reg)
            unboxedNum ty reg = mkUnbox (arithTy ty) (translateReg reg)
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

            floatfn fn r = mkCall fn  [mkUnbox floatTy $ translateReg r]

  mkError _ = ASTError

  mkBigLit _ i = show i

  compileAlloc info indent (ASTAlloc typename name val) =
    case val of Nothing   -> decl
                Just expr -> decl `T.append` " = " `T.append` compile' info indent expr
                where
                  decl = case typename of Nothing -> T.pack ("var " ++ name)
                                          Just t  -> T.pack ("var " ++ name ++ " " ++ t)

  compileError info indent (ASTError exc) = compile info (mkCall "Println" [ASTString exc])

-------------------------------------------------------------------------------

vm        = "vm"
baseType  = "uintptr"
oldbase   = "oldbase"
myoldbase = "myoldbase"

mkVm        = ASTIdent vm
mkStack     = ASTPtrProj mkVm "ValueStack"
mkCallstack = ASTPtrProj mkVm "CallStack"
mkStackbase = ASTPtrProj mkVm "ValueStackBase"
mkStacktop  = ASTPtrProj mkVm "ValueStackTop"
mkRet       = ASTPtrProj mkVm "ReturnValue"
mkOldbase   = ASTIdent oldbase
mkMyOldbase = ASTIdent myoldbase
mkNull      = ASTIdent nullptr

mkLoc 0 = ASTIndex mkStack mkStackbase
mkLoc n = ASTIndex mkStack (mkAdd mkStackbase (ASTNum (ASTInt n)))

mkTop 0 = ASTIndex mkStack mkStacktop
mkTop n = ASTIndex mkStack (mkAdd mkStacktop (ASTNum (ASTInt n)))

mkPush args = ASTApp (ASTProj mkCallstack "push") args
mkPop       = ASTBinOp ";" (mkMeth mkCallstack "top" []) (mkMeth mkCallstack "pop" [])

-- mkIsCon :: ASTNode -> ASTNode
-- mkIsCon obj = mkAnd obj (mkEq (mkPtrMeth obj "getTypeId" []) (ASTIdent "Con::typeId"))

fnParams :: [String]
fnParams = [vm ++ " *VirtualMachine", oldbase ++ " " ++ baseType]

mkUnbox :: String -> ASTNode -> ASTNode
mkUnbox typ obj =  ASTProj (ASTProj (mkCall "ValueOf" [obj]) "Interface()") ("(" ++ typ ++ ")")

translatedType :: ASTNode -> String
translatedType e = case e of
                  (ASTString _)                       -> stringTy
                  (ASTNum (ASTFloat _))               -> floatTy
                  (ASTNum (ASTInteger (ASTBigInt _))) -> bigIntTy
                  (ASTNum _)                          -> intTy
                  (ASTChar _)                         -> charTy
                  (ASTWord (ASTWord8 _))              -> wordTy 8
                  (ASTWord (ASTWord16 _))             -> wordTy 16
                  (ASTWord (ASTWord32 _))             -> wordTy 32
                  (ASTWord (ASTWord64 _))             -> wordTy 64
                  _                                   -> ""

mkAsString :: ASTNode -> ASTNode
mkAsString value = mkCall "Sprint" [value]

mkAsIntegral :: ASTNode -> ASTNode
mkAsIntegral obj = mkMeth (mkCall "ValueOf" [obj]) "Uint" []

mkCast :: String -> ASTNode -> ASTNode
mkCast typ expr = mkCall typ [expr]

mkBoolToInt :: ASTNode -> ASTNode
mkBoolToInt b = mkCall "BoolToInt" [b]

-----------------------------------------------------------------------------------------------------------------------
mkBigInt n = mkCall "big.NewInt" [n]

mkBigIntStr :: String -> ASTNode
mkBigIntStr n = mkMeth (mkBigInt mkZero) "SetString" [ASTString n, mkInt 10]

unboxBig :: Reg -> ASTNode
unboxBig r = mkUnbox bigIntTy $ translateReg r

mkBigBinOp :: Reg -> Reg -> Reg -> String -> ASTNode
mkBigBinOp dest lhs rhs op = mkMeth (unboxBig dest) op [unboxBig lhs, unboxBig rhs]

mkBigBinOp' :: Reg -> Reg -> Reg -> String -> ASTNode
mkBigBinOp' dest lhs rhs op = ASTAssign (translateReg dest) $ mkMeth (mkBigInt mkZero) op [unboxBig lhs, unboxBig rhs]
-----------------------------------------------------------------------------------------------------------------------

nullptr      = "nil"
intTy        = "int"
bigIntTy     = "*big.Int"
floatTy      = "float64"
stringTy     = "string"
charTy       = "byte" -- TODO: switch to "rune" and unicode functions
managedPtrTy = "*interface{}" -- TODO: placeholder
ptrTy        = "Ptr"
conTy        = "Con"
fileTy       = "*File"

wordTy :: Int -> String
wordTy n = PF.printf "uint%d" n
