{-# LANGUAGE OverloadedStrings #-}

module IRTS.CodegenGo (codegenGo) where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import IRTS.Generic.AST
import IRTS.Generic.CodegenGeneric
import IRTS.Generic.CodegenFFI
import Idris.Core.TT
import Util.System hiding (tempfile)

import Numeric
import Data.Char
import Data.Int
import Data.Word
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import System.Process
import System.Exit
import System.IO
import System.Directory
import System.FilePath ((</>), normalise)
import Control.Monad.State
import Control.Arrow

import Debug.Trace

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Printf as PF

import Paths_idris_go

data CompileGo = CompileGo Bool -- TODO: just a placeholder

codegenGo :: CodeGenerator
codegenGo ci =
  codegenGo_all (simpleDecls ci)
                 (outputType ci)
                 (outputFile ci)
                 (includes ci)
                 [] -- objc (currently unused)
                 [] -- libs (currently unused)
                 (compilerFlags ci) -- compiler flags
                 (debugLevel ci) -- (currently unused)
codegenGo_all ::
     [(Name, SDecl)] -> -- declarations/definitions
     OutputType ->      -- output type
     FilePath ->        -- output file name
     [FilePath] ->      -- include files
     String ->          -- extra object files`as
     String ->          -- libraries
     [String] ->        -- extra compiler flags
     DbgLevel ->        -- debug level
     IO ()

codegenGo_all definitions outputType filename includes objs libs flags dbg = do
  let info = CompileGo True
  let bytecode = map toBC definitions
  let go = concatMap (toGo info) bytecode
  path <- getDataDir
  let goOut = (   package (map T.pack flags)
                  `T.append` "\n\n"
                  `T.append` mkImport ". reflect"
                  `T.append` mkImport ". os"
                  `T.append` mkImport ". unicode/utf8"
                  `T.append` mkImport ". fmt"
                  `T.append` mkImport ". math"
                  `T.append` mkImport "  math/big"
                  `T.append` mkImport ". idris_runtime"
                  `T.append` "\n"
                  `T.append` imports includes
                  `T.append` "\n"
                  `T.append` T.concat (map (compile info) go)
                  `T.append` mkIgnoreUnusedImports
                  `T.append` "\n"
                  `T.append` mkMain
                  `T.append` "\n"
               )
  case outputType of
    Raw -> TIO.writeFile filename goOut
    _ -> do (tmpn, tmph) <- tempfile
            hPutStr tmph (T.unpack goOut)
            hFlush tmph
            hClose tmph
            let cc =
                     "GOPATH=${GOPATH}:" ++ path ++ "; " ++
                     "go build -o " ++ filename ++ " " ++ tmpn
            exit <- system cc
            when (exit /= ExitSuccess) $
              putStrLn ("FAILURE: " ++ cc)
    where
      mkImport s = case qual of "//" -> s `T.append` "\n"
                                _    -> "import " `T.append` imp
        where
          ws = T.words s
          qual = case ws of (w:_:_) -> w
                            [w]     -> " "
          pkg = last ws
          imp = qual `T.append` " \"" `T.append` pkg `T.append` "\"\n"

      mkIgnoreUnusedImports = T.pack (foldr (++) "\n" (map ("\nconst _ = " ++) consts)) `T.append`
                              T.pack (foldr (++) "\n" (map ("\nvar _ " ++) types))
        where consts = ["SelectDefault", "UTFMax", "Pi", "big.MaxBase", "DevNull"]
              types = ["State"]

      imports xs = T.concat (map (mkImport . T.pack) (reverse xs))

      package :: [T.Text] -> T.Text
      package fs = fromMaybe "package main" (find (T.isPrefixOf "package ") fs)

      mkMain = T.pack $ "func main() {\n" ++
                        "  " ++ vm ++ " := " ++ vmType ++ "{}\n" ++
                        "  Call(&" ++ vm ++ ", " ++ vmMainFn ++ ", 0)\n" ++
                        "}\n" ++
                        "var Main = main"
toGo info (name, bc) =
  [ ASTIdent $ "func " ++ translateName name,
    ASTFunction fnParams (
      ASTSeq $ ASTAlloc (Just baseType) myoldbase Nothing
               : ASTAssign (ASTIdent "_") mkMyOldbase
               : map (translateBC info)bc
    )
  ]

tempfile :: IO (FilePath, Handle)
tempfile = do dir <- getTemporaryDirectory
              openTempFile (normalise dir) "idris.go"

translateReg :: Reg -> ASTNode
translateReg reg =
  case reg of
    RVal -> mkRet
    Tmp  -> ASTRaw "//TMPREG"
    L n  -> mkLoc n
    T n  -> mkTop n

---------------------------------------------------------------------------------------------------
instance CompileInfo CompileGo where
---------------------------------------------------------------------------------------------------
  mkAssign _ r1 r2 = ASTAssign (translateReg r1) (translateReg r2)

  mkAssignConst _ r c =
    case value of
      ASTNum (ASTInteger (ASTBigInt i)) -> assignBigValue i
      _                                 -> ASTAssign (translateReg r) (mkCast (translatedType value) value)
      where
        value = translateConstant c
        assignBigValue i
          | i > (toInteger (maxBound::Word64)) ||
            i < (toInteger (minBound::Int64)) = ASTAssign (translateReg r) (mkStringToBigInt (ASTString $ show i))
          | i > (toInteger (maxBound::Int64)) = ASTAssign (translateReg r) (mkNewBigUInt i)
          | otherwise = ASTAssign (translateReg r) (mkNewBigInt i)

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
                             (ASTBinOp ";" mkNull (mkCall "Print" [asType stringTy $ translateReg str]))

      "putchar" -> let [(_, ch)] = args in
                   ASTAssign (translateReg reg)
                             (ASTBinOp ";" mkNull (mkCall "Printf" [ASTString "%c",
                                                                    asType charTy $ translateReg ch]))

      "getchar" -> mkCall "Scanf" [ASTString "%c", asType charTy $ translateReg reg]

      "fileOpen" -> let [(_, name),(_, mode)] = args in
                    ASTAssign (translateReg reg)
                              (mkCall "FileOpen" [asType stringTy $ translateReg name,
                                                  asType stringTy $ translateReg mode])
      "fileClose" -> let [(_, fh)] = args in
                     ASTAssign (translateReg reg) (mkMeth (asType fileTy $ translateReg fh) "Close" [])

      "fputStr" -> let [(_, fh),(_, str)] = args in
                   mkAssignFirst (translateReg reg)
                                 (mkMeth (asType fileTy $ translateReg fh)
                                         "WriteString"
                                         [asType stringTy $ translateReg str])
      "fileEOF" -> let [(_, fh)] = args in
                   ASTAssign (translateReg reg) (mkCall "FileEOF" [asType fileTy $ translateReg fh])

      "fileError" -> let [(_, fh)] = args in error "fileError not supported yet"

      "isNull" -> let [(_, arg)] = args in
                  ASTAssign (translateReg reg) (mkBoolToInt $ mkEq (translateReg arg) mkNull)

      "idris_eqPtr" -> let [(_, lhs),(_, rhs)] = args in
                    ASTAssign (translateReg reg) (mkBoolToInt $ mkEq (translateReg lhs) (translateReg rhs))

      "getenv" -> ASTCond [(ASTIdent "true", ASTSeq [getEnv, getEnvResults])]
                  where
                    [(_, arg)] = args
                    getEnv = ASTAssign (translateReg reg) (mkCall "Getenv" [asType stringTy $ translateReg arg])
                    getEnvResults = ASTCond [(mkEq (asType stringTy $ translateReg reg) (ASTIdent "\"\""),
                                              ASTSeq [ ASTAssign (translateReg reg) mkNull])]

      "exit" -> mkCall "Exit" [asType intTy $ translateReg reg]

      "idris_numArgs" -> ASTAssign (translateReg reg) (mkCall "len" [ASTIdent "Args"])
      "idris_getArg"  -> let [(_, arg)] = args in
                         ASTAssign (translateReg reg) (ASTIndex (ASTIdent "Args")
                                   (asType intTy $ translateReg arg))

      _ -> ASTAssign (translateReg reg) (let callexpr = ASTFFI n (map generateWrapper args) in
                                         case ret of
                                           FUnit -> ASTBinOp ";" mkNull callexpr
                                           _     -> callexpr)
      where
        generateWrapper :: (FType, Reg) -> ASTNode
        generateWrapper (ty, reg) =
          case ty of
            FFunction -> ffunc FUnit FUnit
            FFunctionIO -> ffunc FUnit FUnit
            _ -> asType (head $ goType ty) (translateReg reg)

          where ffunc aty rty = let rs = goType rty in
                                genClosure reg (genArgs (goType aty ++ init rs)) (last $ rs)

        goType :: FType -> [String]
        goType (FArith (ATInt ITNative))       = [intTy]
        goType (FArith (ATInt ITChar))         = [charTy]
        goType (FArith (ATInt ITBig))          = [bigIntTy]
        goType (FArith (ATInt (ITFixed IT8)))  = [wordTy  8]
        goType (FArith (ATInt (ITFixed IT16))) = [wordTy 16]
        goType (FArith (ATInt (ITFixed IT32))) = [wordTy 32]
        goType (FArith (ATInt (ITFixed IT64))) = [wordTy 64]
        goType FString                         = [stringTy]
        goType FUnit                           = [""]
        goType FPtr                            = ["interface{}"]
        goType FManagedPtr                     = ["interface{}"] -- TODO: placeholder
        goType (FArith ATFloat)                = [floatTy]
        goType FAny                            = ["interface{}"]
        goType FFunction                       = concat [goType FUnit, goType FUnit]

        genArgs :: [String] -> [(String, String)]
        genArgs typs = zip ((map (\n -> "arg" ++ show n)) [1..]) typs

        genClosure :: Reg -> [(String, String)] -> String -> ASTNode
        genClosure con xs r = ASTRaw $ "func(" ++ intercalate ", " (map (genDecl) xs) ++ ") " ++ r ++ " " ++
          "{" ++
            "ProxyFunction(" ++
            vm ++ ", " ++
            vmApplyFn ++ ", " ++
            T.unpack (compile' info 0 $ translateReg con) ++ ", " ++
            (intercalate "," (map fst xs)) ++ "); " ++
            retIfNeeded ++
          "}"
          where retIfNeeded = if r == "" then "" else T.unpack (compile' info 0 (ASTReturn (asType r mkRet)))

        genDecl :: (String, String) -> String
        genDecl (v,t) = v ++ " " ++ t

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
      map (binOp (mkEq) (translateReg reg) . translateConstant *** prepBranch) cases
    ) ++ (maybe [] ((:[]) . ((,) ASTNoop) . prepBranch) def)
      where
        prepBranch :: [BC] -> ASTNode
        prepBranch bc = ASTSeq $ map (translateBC info) bc

        binOp :: (ASTNode -> ASTNode -> ASTNode) -> ASTNode -> ASTNode -> ASTNode
        binOp f l r = case r of
                        (ASTNum (ASTInteger (ASTBigInt i))) -> eqCheck (asType bigIntTy l) (mkBig i)
                        _ -> f (asType (translatedType r) l) r
                        where
                          eqCheck lhs rhs = mkEq (mkMeth lhs "Cmp" [rhs]) mkZero
                          mkBig i
                            | i == 0 = ASTRaw "ConstBigZero"
                            | i == 1 = ASTRaw "ConstBigOne"
                            | i > (toInteger (maxBound::Word64)) ||
                              i < (toInteger (minBound::Int64)) = mkStringToBigInt (ASTString $ show i)
                            | i > (toInteger (maxBound::Int64)) = mkNewBigUInt i
                            | otherwise = mkNewBigInt i

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

  mkOp _ reg (LTrunc ITBig (ITFixed IT64)) (arg:_) =
    ASTCond [(ASTIdent "true", ASTSeq [
              ASTAlloc Nothing tmpVarName (Just (mkNewBigUIntStr "0xFFFFFFFFFFFFFFFF")),
              ASTAssign (translateReg reg)
                        (mkMeth (mkMeth tmpVar "And" [tmpVar, asBig arg]) "Uint64" [])])]
    where tmpVarName = "tmpBig"
          tmpVar = ASTIdent tmpVarName

  mkOp _ reg oper args = ASTAssign (translateReg reg) (mkOp' oper)
    where
      mkOp' :: PrimFn -> ASTNode
      mkOp' op =
        case op of
          LNoOp -> translateReg (last args)

          (LZExt sty ITBig) -> mkNewBigInt' (mkAsInt . translateReg $ last args)
          (LZExt sty dty) -> mkIntCast dty $ asInt sty (last args)
          (LSExt sty dty) -> mkOp' (LZExt sty dty)

          (LPlus  (ATInt ITBig)) -> mkMeth mkAllocBigInt "Add" [asBig lhs, asBig rhs]
          (LMinus (ATInt ITBig)) -> mkMeth mkAllocBigInt "Sub" [asBig lhs, asBig rhs]
          (LTimes (ATInt ITBig)) -> mkMeth mkAllocBigInt "Mul" [asBig lhs, asBig rhs]
          (LSDiv  (ATInt ITBig)) -> mkMeth mkAllocBigInt "Div" [asBig lhs, asBig rhs]
          (LSRem  (ATInt ITBig)) -> mkMeth mkAllocBigInt "Rem" [asBig lhs, asBig rhs]

          (LPlus ty)  -> mkAdd      (asNum ty lhs) (asNum ty rhs)
          (LMinus ty) -> mkSubtract (asNum ty lhs) (asNum ty rhs)
          (LTimes ty) -> mkMultiply (asNum ty lhs) (asNum ty rhs)
          (LSDiv ty)  -> mkDivide   (asNum ty lhs) (asNum ty rhs)
          (LSRem ty)  -> mkModulo   (asNum ty lhs) (asNum ty rhs)

          (LEq  (ATInt ITBig)) -> mkBitXor (mkBitAnd (mkMeth (asBig lhs) "Cmp" [asBig rhs]) mkOne) mkOne
          (LSLt (ATInt ITBig)) -> mkBoolToInt $ mkLessThan      (mkMeth (asBig lhs) "Cmp" [asBig rhs]) mkZero
          (LSLe (ATInt ITBig)) -> mkBoolToInt $ mkLessThanEq    (mkMeth (asBig lhs) "Cmp" [asBig rhs]) mkZero
          (LSGt (ATInt ITBig)) -> mkBoolToInt $ mkGreaterThan   (mkMeth (asBig lhs) "Cmp" [asBig rhs]) mkZero
          (LSGe (ATInt ITBig)) -> mkBoolToInt $ mkGreaterThanEq (mkMeth (asBig lhs) "Cmp" [asBig rhs]) mkZero

          (LEq ty)  -> mkBoolToInt $ mkEq            (asNum ty lhs) (asNum ty rhs)
          (LSLt ty) -> mkBoolToInt $ mkLessThan      (asNum ty lhs) (asNum ty rhs)
          (LSLe ty) -> mkBoolToInt $ mkLessThanEq    (asNum ty lhs) (asNum ty rhs)
          (LSGt ty) -> mkBoolToInt $ mkGreaterThan   (asNum ty lhs) (asNum ty rhs)
          (LSGe ty) -> mkBoolToInt $ mkGreaterThanEq (asNum ty lhs) (asNum ty rhs)

          (LTrunc ITNative (ITFixed IT8))        -> mkTrunc intTy        8  "0xFF"
          (LTrunc (ITFixed IT16) (ITFixed IT8))  -> mkTrunc (wordTy 16)  8  "0xFF"
          (LTrunc (ITFixed IT32) (ITFixed IT16)) -> mkTrunc (wordTy 32) 16  "0xFFFF"
          (LTrunc (ITFixed IT64) (ITFixed IT32)) -> mkTrunc (wordTy 64) 32  "0xFFFFFFFF"

          (LTrunc ITBig ITNative) -> mkCast (intTy) (mkMeth (asBig arg) "Int64" [])

          (LLSHR ty@(ITFixed _)) -> mkOp' (LASHR ty)
          (LLt ty@(ITFixed _))   -> mkOp' (LSLt (ATInt ty))
          (LLe ty@(ITFixed _))   -> mkOp' (LSLe (ATInt ty))
          (LGt ty@(ITFixed _))   -> mkOp' (LSGt (ATInt ty))
          (LGe ty@(ITFixed _))   -> mkOp' (LSGe (ATInt ty))
          (LUDiv ty@(ITFixed _)) -> mkOp' (LSDiv (ATInt ty))

          (LAnd ty)   -> mkIntCast ty $ mkBitAnd (asInt ty lhs) (asInt ty rhs)
          (LOr ty)    -> mkIntCast ty $ mkBitOr  (asInt ty lhs) (asInt ty rhs)
          (LXOr ty)   -> mkIntCast ty $ mkBitXor (asInt ty lhs) (asInt ty rhs)
          (LSHL ty)   -> mkIntCast ty $ mkBitShl (asInt ty lhs) (mkAsUInt $ translateReg rhs)
          (LASHR ty)  -> mkIntCast ty $ mkBitShr (asInt ty lhs) (mkAsUInt $ translateReg rhs)
          (LCompl ty) -> mkIntCast ty $ mkBitCompl (asInt ty arg)

          LStrConcat -> mkAdd (asString lhs) (asString rhs)
          LStrEq     -> mkBoolToInt $ mkEq (asString lhs) (asString rhs)
          LStrLt     -> mkBoolToInt $ mkLessThan (asString lhs) (asString rhs)
          LStrLen    -> mkCall "RuneCountInString" [asString arg]

          (LStrInt ITNative)     -> mkCast intTy $ mkCall "StringToInt" [asString arg]
          (LIntStr ITNative)     -> mkToString $ translateReg arg
          (LIntStr ITBig)        -> mkMeth (asBig arg) "String" []
          (LStrInt ITBig)        -> mkStringToBigInt (asString arg)
          LFloatStr              -> mkToString $ translateReg arg
          LStrFloat              -> mkCall "StringToFloat" [asString arg]

          (LIntFloat ITNative)   -> mkCast floatTy (asType intTy $ translateReg arg)
          (LFloatInt ITNative)   -> mkCast intTy   (asType floatTy $ translateReg arg)
          (LChInt ITNative)      -> mkCast intTy   (asType charTy $ translateReg arg)
          (LIntCh ITNative)      -> mkCast charTy  (asType intTy $ translateReg arg)

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

          LStrCons  -> mkCall "Sprintf" [ASTString "%c%s", asType charTy $ translateReg lhs, asString rhs]

          LStrHead  -> mkCall "Utf8Head" [asString arg]

          LStrRev   -> mkCall "Utf8Reverse" [asString arg]

          LStrIndex -> mkCall "Utf8AtIndex" [asString lhs, asType intTy $ translateReg rhs]

          LStrTail  -> mkCall "Utf8Tail" [asString arg]

          LReadStr    -> mkCall "FileReadLine" [asType fileTy $ translateReg arg]
          LSystemInfo -> ASTString "golang backend (stub version info)"
          LNullPtr    -> mkNull

          LStdIn  -> ASTIdent "Stdin"
          LStdOut -> ASTIdent "Stdout"
          LStdErr -> ASTIdent "Stderr"

          _ -> error ("Not implemented: " ++ show op)

          where
            (lhs:rhs:_) = args
            (arg:_) = args

            mkTrunc src dst mask =
              mkCast (wordTy dst) (mkBitAnd (asType src $ translateReg arg) (ASTRaw mask))

            mkIntCast ty expr = mkCast (arithTy (ATInt ty)) expr

            asString reg = asType stringTy (translateReg reg)
            asNum ty reg = asType (arithTy ty) (translateReg reg)
            asInt ty reg = asType (arithTy (ATInt ty)) (translateReg reg)

            arithTy (ATInt ITNative)       = intTy
            arithTy (ATInt ITChar)         = charTy
            arithTy (ATFloat)              = floatTy
            arithTy (ATInt (ITFixed IT8))  = wordTy 8
            arithTy (ATInt (ITFixed IT16)) = wordTy 16
            arithTy (ATInt (ITFixed IT32)) = wordTy 32
            arithTy (ATInt (ITFixed IT64)) = wordTy 64
            arithTy (ty)                   = error ("UNKNOWN TYPE: " ++ show ty)

            floatfn fn r = mkCall fn  [asType floatTy $ translateReg r]

  mkError _ = ASTError

  mkBigLit _ i = show i

  lineTerminator _ = ""
  condBraces _ = ("","")

  compileAlloc info indent (ASTAlloc typename name val) =
    case val of Nothing   -> decl
                Just expr -> decl `T.append` " = " `T.append` compile' info indent expr
                where
                  decl = case typename of Nothing -> T.pack ("var " ++ name)
                                          Just t  -> T.pack ("var " ++ name ++ " " ++ t)

  compileError info indent (ASTError exc) = compile info (mkCall "Println" [ASTString exc])

---------------------------------------------------------------------------------------------------

vm        = "vm"
baseType  = "uintptr"
oldbase   = "oldbase"
myoldbase = "myoldbase"

vmType ="VirtualMachine"
vmApplyFn = "_idris__123_APPLY0_125_"
vmMainFn = "_idris__123_runMain0_125_"

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

fnParams :: [String]
fnParams = [vm ++ " *" ++ vmType ++ ", " ++ oldbase ++ " " ++ baseType]

asType :: String -> ASTNode -> ASTNode
asType typ obj =  ASTProj obj ("(" ++ typ ++ ")")

translatedType :: ASTNode -> String
translatedType e = case e of
                  (ASTString _)           -> stringTy
                  (ASTNum (ASTFloat _))   -> floatTy
                  (ASTNum _)              -> intTy
                  (ASTChar _)             -> charTy
                  (ASTWord (ASTWord8 _))  -> wordTy 8
                  (ASTWord (ASTWord16 _)) -> wordTy 16
                  (ASTWord (ASTWord32 _)) -> wordTy 32
                  (ASTWord (ASTWord64 _)) -> wordTy 64
                  (ASTType ASTIntTy)      -> intTy
                  (ASTType ASTStringTy)   -> stringTy
                  (ASTType ASTIntegerTy)  -> bigIntTy
                  (ASTType ASTFloatTy)    -> floatTy
                  (ASTType ASTCharTy)     -> charTy
                  (ASTType ASTPtrTy)      -> ptrTy
                  _                       -> ""

mkToString :: ASTNode -> ASTNode
mkToString value = mkCall "Sprint" [value]

mkAsInt :: ASTNode -> ASTNode
mkAsInt obj = mkMeth (mkCall "ValueOf" [obj]) "Int" []

mkAsUInt :: ASTNode -> ASTNode
mkAsUInt obj = mkMeth (mkCall "ValueOf" [obj]) "Uint" []

mkCast :: String -> ASTNode -> ASTNode
mkCast typ expr = mkCall typ [expr]

mkBoolToInt :: ASTNode -> ASTNode
mkBoolToInt b = mkCall "BoolToInt" [b]

ignoreSecond :: ASTNode -> ASTNode
ignoreSecond arg = ASTBinOp "," arg (ASTIdent "_")

mkAssignFirst :: ASTNode -> ASTNode -> ASTNode
mkAssignFirst lhs rhs = ASTAssign (ignoreSecond lhs) rhs

mkAllocBigInt :: ASTNode
mkAllocBigInt = mkCall "new" [ASTIdent "big.Int"]

mkNewBigInt' :: ASTNode -> ASTNode
mkNewBigInt' n = mkCall "big.NewInt" [n]

mkNewBigInt :: Integer -> ASTNode
mkNewBigInt n = mkNewBigInt' (mkBigInt n)

mkNewBigUInt' :: ASTNode -> ASTNode
mkNewBigUInt' n = mkMeth mkAllocBigInt "SetUint64" [n]

mkNewBigUInt :: Integer -> ASTNode
mkNewBigUInt n = mkNewBigUInt' (mkBigInt n)

mkNewBigUIntStr :: String -> ASTNode
mkNewBigUIntStr n = mkNewBigUInt' (ASTRaw n)

mkStringToBigInt :: ASTNode -> ASTNode
mkStringToBigInt n = mkCall "StringToBigInt" [n]

asBig :: Reg -> ASTNode
asBig r = asType bigIntTy $ translateReg r

nullptr      = "nil"
intTy        = "int"
bigIntTy     = "*big.Int"
floatTy      = "float64"
stringTy     = "string"
charTy       = "rune"
ptrTy        = "Ptr"
conTy        = "Con"
fileTy       = "*File"

wordTy :: Int -> String
wordTy n = PF.printf "uint%d" n
