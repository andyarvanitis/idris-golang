{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenGeneric where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import IRTS.CodegenFFI
import IRTS.AST
import Idris.Core.TT

import Numeric
import Data.Char
import Data.Word

import qualified Data.Text as T
import qualified Text.Printf as PF


class CompileInfo a where
  mkAssign :: a -> Reg -> Reg -> ASTNode
  mkAssignConst :: a -> Reg -> Const -> ASTNode
  mkAddTop :: a -> Int -> ASTNode
  mkNullAssign :: a -> Reg -> ASTNode
  mkVmCall :: a -> Name -> ASTNode
  mkVmTailCall :: a -> Name -> ASTNode
  mkForeign :: a -> Reg -> String -> [(FType, Reg)] -> FType -> ASTNode
  mkTopBase :: a -> Int -> ASTNode
  mkBaseTop :: a -> Int -> ASTNode
  mkStoreOld :: a -> ASTNode
  mkSlide :: a -> Int -> ASTNode
  mkRebase :: a -> ASTNode
  mkReserve :: a -> Int -> ASTNode
  mkMakeCon :: a -> Reg -> Int -> [Reg] -> ASTNode
  mkConstCase :: a -> Reg -> [(Const, [BC])] -> Maybe [BC] -> ASTNode
  mkCase :: a -> Bool -> Reg -> [(Int, [BC])] -> Maybe [BC] -> ASTNode  
  mkProject :: a -> Reg -> Int -> Int -> ASTNode
  mkOp :: a -> Reg -> PrimFn -> [Reg] -> ASTNode
  mkError :: a -> String -> ASTNode


translateConstant :: Const -> ASTNode
translateConstant (I i)                    = ASTNum (ASTInt i)
translateConstant (Fl f)                   = ASTNum (ASTFloat f)
translateConstant (Ch c)                   = ASTChar (translateChar c)
translateConstant (Str s)                  = ASTString $ concatMap translateChar s
translateConstant (AType (ATInt ITNative)) = ASTType ASTIntTy
translateConstant StrType                  = ASTType ASTStringTy
translateConstant (AType (ATInt ITBig))    = ASTType ASTIntegerTy
translateConstant (AType ATFloat)          = ASTType ASTFloatTy
translateConstant (AType (ATInt ITChar))   = ASTType ASTCharTy
translateConstant PtrType                  = ASTType ASTPtrTy
translateConstant Forgot                   = ASTType ASTForgotTy
translateConstant (BI i)                   = ASTNum $ ASTInteger (ASTBigInt i)
translateConstant (B8 b)                   = ASTWord (ASTWord8 b)
translateConstant (B16 b)                  = ASTWord (ASTWord16 b)
translateConstant (B32 b)                  = ASTWord (ASTWord32 b)
translateConstant (B64 b)                  = ASTWord (ASTWord64 b)
translateConstant c =
  ASTError $ "Unimplemented Constant: " ++ show c
  
translateChar :: Char -> String
translateChar ch
  | isAscii ch && isAlphaNum ch  = [ch]
  | ch `elem` [' ','_', ',','.'] = [ch]
  | otherwise                    = mkCodepoint (ord ch)

mkCodepoint :: Int -> String
mkCodepoint c = PF.printf "\\U%.8X" c


translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"


translateBC :: CompileInfo a => a -> BC -> ASTNode
translateBC info bc =
  case bc of
    ASSIGN r1 r2          ->  mkAssign info r1 r2
    ASSIGNCONST r c       ->  mkAssignConst info r c
    UPDATE r1 r2          ->  mkAssign info r1 r2
    ADDTOP n              ->  mkAddTop info n
    NULL r                ->  mkNullAssign info r
    CALL n                ->  mkVmCall info n
    TAILCALL n            ->  mkVmTailCall info n
    FOREIGNCALL r _ t n a ->  mkForeign info r n a t
    TOPBASE n             ->  mkTopBase info n
    BASETOP n             ->  mkBaseTop info n
    STOREOLD              ->  mkStoreOld info
    SLIDE n               ->  mkSlide info n
    REBASE                ->  mkRebase info
    RESERVE n             ->  mkReserve info n
    MKCON r _ t rs        ->  mkMakeCon info r t rs
    CASE s r c d          ->  mkCase info s r c d
    CONSTCASE r c d       ->  mkConstCase info r c d
    PROJECT r l a         ->  mkProject info r l a
    OP r o a              ->  mkOp info r o a
    ERROR e               ->  mkError info e
    _                     ->  ASTRaw $ "//" ++ show bc

-------------------------------------------------------------------------------

compile :: ASTNode -> T.Text
compile = compile' 0

compile' :: Int -> ASTNode -> T.Text
compile' indent ASTNoop = ""

compile' indent (ASTAnnotation annotation expr) =
   ""
  `T.append` T.pack (show annotation)
  `T.append` " */\n"
  `T.append` compile' indent expr

compile' indent (ASTFFI raw args) =
  ffi raw (map (T.unpack . compile' indent) args)

compile' indent (ASTRaw code) =
  T.pack code

compile' indent (ASTIdent ident) =
  T.pack ident

compile' indent (ASTFunction args body) =
   T.replicate indent " " `T.append` "("
   `T.append` T.intercalate "," (map T.pack args)
   `T.append` ") {\n"
   `T.append` compile' (indent + 2) body
   `T.append` "\n}\n"

compile' indent (ASTSeq seq) =
  T.intercalate ";\n" (
    map (
      (T.replicate indent " " `T.append`) . (compile' indent)
    ) $ filter (/= ASTNoop) seq
  ) `T.append` ";"

compile' indent (ASTList seq) =
  T.intercalate "," (
    map (
      (T.replicate indent " " `T.append`) . (compile' indent)
    ) $ filter (/= ASTNoop) seq
  )

compile' indent (ASTReturn val) =
  "return " `T.append` compile' indent val

compile' indent (ASTApp lhs rhs)
  | ASTFunction {} <- lhs =
    T.concat ["(", compile' indent lhs, ")(", args, ")"]
  | otherwise =
    T.concat [compile' indent lhs, "(", args, ")"]
  where args :: T.Text
        args = T.intercalate "," $ map (compile' 0) rhs

compile' indent (ASTNew name args) =
  T.pack name
  `T.append` "("
  `T.append` T.intercalate "," (map (compile' 0) args)
  `T.append` ")"

compile' indent (ASTError exc) = compile (mkCall "putStr" [ASTString exc]) `T.append` "; assert(false)"

compile' indent (ASTBinOp op lhs rhs) =
    compile' indent lhs
  `T.append` " "
  `T.append` T.pack op
  `T.append` " "
  `T.append` compile' indent rhs

compile' indent (ASTPreOp op val) =
  T.pack op `T.append` compile' indent val

compile' indent (ASTPostOp op val) =
  compile' indent val `T.append` T.pack op

compile' indent (ASTProj obj field)
  | ASTFunction {} <- obj =
    T.concat ["(", compile' indent obj, ").", T.pack field]
  | ASTAssign {} <- obj =
    T.concat ["(", compile' indent obj, ").", T.pack field]
  | otherwise =
    compile' indent obj `T.append` "." `T.append` T.pack field

compile' indent (ASTPtrProj obj field)
  | ASTFunction {} <- obj =
    T.concat ["(", compile' indent obj, ")->", T.pack field]
  | ASTAssign {} <- obj =
    T.concat ["(", compile' indent obj, ")->", T.pack field]
  | otherwise =
    compile' indent obj `T.append` "->" `T.append` T.pack field

compile' indent (ASTArray elems) =
  "{" `T.append` T.intercalate "," (map (compile' 0) elems) `T.append` "}"

compile' indent (ASTString str) =
  "\"" `T.append` T.pack str `T.append` "\""

compile' indent (ASTChar c) =
  "'" `T.append` T.pack c `T.append` "'"

compile' indent (ASTNum num) =
  case num of
    ASTInt i                     -> T.pack (show i)
    ASTFloat f                   -> T.pack (show f)
    ASTInteger (ASTBigInt i)     -> T.pack $ big i
    ASTInteger (ASTBigIntExpr e) -> compile' indent e
  where
    big :: Integer -> String
    big i
      | i > (toInteger (maxBound::Int)) || i < (toInteger (minBound::Int)) = "asBig(" ++ (show i) ++ ")"
      | otherwise = show i

compile' indent (ASTAssign lhs rhs) =
  compile' indent lhs `T.append` " = " `T.append` compile' indent rhs

compile' 0 (ASTAlloc _ name (Just val@(ASTNew _ _))) =
  T.pack name
  `T.append` " = "
  `T.append` compile' 0 val
  `T.append` ";\n"

compile' indent (ASTAlloc typename name val) =
    case val of Nothing   -> typ `T.append` T.pack name
                Just expr -> typ `T.append` T.pack name `T.append` " = " `T.append` compile' indent expr
                where
                  typ = case typename of Nothing -> T.pack "auto "
                                         Just t  -> T.pack (t ++ " ")
    -- let expr = maybe "" (compile' indent) val
    -- in case val of (Nothing)               -> ""
    --                (Just (ASTFunction _ _)) -> (if name == "main" then "int " else "void ")
    --                                             `T.append` T.pack name `T.append`  expr
    --                (_)                       -> "auto " `T.append` T.pack name `T.append` " = " `T.append` expr

compile' indent (ASTIndex lhs rhs) =
    compile' indent lhs
  `T.append` "["
  `T.append` compile' indent rhs
  `T.append` "]"

compile' indent (ASTCond branches) =
  T.intercalate " else " $ map createIfBlock branches
  where
    createIfBlock (ASTNoop, e@(ASTSeq _)) =
         "{\n"
      `T.append` compile' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (ASTNoop, e) =
         "{\n"
      `T.append` compile' (indent + 2) e
      `T.append` ";\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e@(ASTSeq _)) =
         "if (" `T.append` compile' indent cond `T.append`") {\n"
      `T.append` compile' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e) =
         "if (" `T.append` compile' indent cond `T.append`") {\n"
      `T.append` T.replicate (indent + 2) " "
      `T.append` compile' (indent + 2) e
      `T.append` ";\n"
      `T.append` T.replicate indent " "
      `T.append` "}"

compile' indent (ASTSwitch val [(_,ASTSeq seq)] Nothing) =
  let (h,t) = splitAt 1 seq in
         (T.concat (map (compile' indent) h) `T.append` ";\n")
      `T.append` (
        T.intercalate ";\n" $ map (
          (T.replicate indent " " `T.append`) . compile' indent
        ) t
      )

compile' indent (ASTSwitch val branches def) =
     "switch (" `T.append` compile' indent val `T.append` ") {\n"
  `T.append` T.concat (map mkBranch branches)
  `T.append` mkDefault def
  `T.append` T.replicate indent " " `T.append` "}"
  where
    mkBranch :: (ASTNode, ASTNode) -> T.Text
    mkBranch (tag, code) =
         T.replicate (indent + 2) " "
      `T.append` "case "
      `T.append` compile' indent tag
      `T.append` ":\n"
      `T.append` compile' (indent + 4) code
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"
      `T.append` (T.replicate (indent + 4) " " `T.append` "\n")

    mkDefault :: Maybe ASTNode -> T.Text
    mkDefault Nothing = ""
    mkDefault (Just def) =
         T.replicate (indent + 2) " " `T.append` "default:\n"
      `T.append` compile' (indent + 4)def
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"

compile' indent (ASTTernary cond true false) =
  let c = compile' indent cond
      t = compile' indent true
      f = compile' indent false in
        "("
      `T.append` c
      `T.append` ")?("
      `T.append` t
      `T.append` "):("
      `T.append` f
      `T.append` ")"

compile' indent (ASTParens expr) =
  "(" `T.append` compile' indent expr `T.append` ")"

compile' indent (ASTWhile cond body) =
     "while (" `T.append` compile' indent cond `T.append` ") {\n"
  `T.append` compile' (indent + 2) body
  `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"

compile' indent (ASTWord word)
  | ASTWord8  b <- word = compile' indent (fromInt b)
  | ASTWord16 b <- word = compile' indent (fromInt b)
  | ASTWord32 b <- word = compile' indent (fromInt b)
  | ASTWord64 b <- word = compile' indent (fromBigInt b)
    where
      fromInt n = ASTNum $ ASTInt (fromIntegral n)
      fromBigInt n = ASTNum . ASTInteger . ASTBigInt $ fromIntegral n

mkOr :: ASTNode -> ASTNode -> ASTNode
mkOr lhs rhs = ASTBinOp "||" lhs rhs

mkAnd :: ASTNode -> ASTNode -> ASTNode
mkAnd lhs rhs = ASTBinOp "&&" lhs rhs

mkMeth :: ASTNode -> String -> [ASTNode] -> ASTNode
mkMeth obj meth args = ASTApp (ASTProj obj meth) args

mkPtrMeth :: ASTNode -> String -> [ASTNode] -> ASTNode
mkPtrMeth obj meth args = ASTApp (ASTPtrProj obj meth) args

mkCall :: String -> [ASTNode] -> ASTNode
mkCall fun args = ASTApp (ASTIdent fun) args

mkEq :: ASTNode -> ASTNode -> ASTNode
mkEq lhs rhs = ASTBinOp "==" lhs rhs

mkNotEq :: ASTNode -> ASTNode -> ASTNode
mkNotEq lhs rhs = ASTBinOp "!=" lhs rhs

mkLessThan :: ASTNode -> ASTNode -> ASTNode
mkLessThan lhs rhs = ASTBinOp "<" lhs rhs

mkLessThanEq :: ASTNode -> ASTNode -> ASTNode
mkLessThanEq lhs rhs = ASTBinOp "<=" lhs rhs

mkGreaterThan :: ASTNode -> ASTNode -> ASTNode
mkGreaterThan lhs rhs = ASTBinOp ">" lhs rhs

mkGreaterThanEq :: ASTNode -> ASTNode -> ASTNode
mkGreaterThanEq lhs rhs = ASTBinOp ">=" lhs rhs

mkBitAnd :: ASTNode -> ASTNode -> ASTNode
mkBitAnd lhs rhs = ASTBinOp "&" lhs rhs

mkBitOr :: ASTNode -> ASTNode -> ASTNode
mkBitOr lhs rhs = ASTBinOp "|" lhs rhs

mkBitXor :: ASTNode -> ASTNode -> ASTNode
mkBitXor lhs rhs = ASTBinOp "^" lhs rhs

mkBitShl :: ASTNode -> ASTNode -> ASTNode
mkBitShl lhs rhs = ASTBinOp "<<" lhs rhs

mkBitShr :: ASTNode -> ASTNode -> ASTNode
mkBitShr lhs rhs = ASTBinOp ">>" lhs rhs

mkBitCompl :: ASTNode -> ASTNode
mkBitCompl n = ASTPreOp "~" n

mkZero :: ASTNode
mkZero = ASTNum (ASTInt 0)

mkOne :: ASTNode
mkOne = ASTNum (ASTInt 1)

mkAdd :: ASTNode -> ASTNode -> ASTNode
mkAdd lhs rhs = ASTBinOp "+" lhs rhs

mkSubtract :: ASTNode -> ASTNode -> ASTNode
mkSubtract lhs rhs = ASTBinOp "-" lhs rhs

mkMultiply :: ASTNode -> ASTNode -> ASTNode
mkMultiply lhs rhs = ASTBinOp "*" lhs rhs

mkDivide :: ASTNode -> ASTNode -> ASTNode
mkDivide lhs rhs = ASTBinOp "/" lhs rhs

mkModulo :: ASTNode -> ASTNode -> ASTNode
mkModulo lhs rhs = ASTBinOp "%" lhs rhs

