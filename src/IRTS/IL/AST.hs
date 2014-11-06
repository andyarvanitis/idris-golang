{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module IRTS.IL.AST where

import Data.Word
import Data.Char (isDigit)
import Data.List (intersperse)
import Data.Int

import qualified Data.Text as T

data ILType = ILIntTy
            | ILStringTy
            | ILIntegerTy
            | ILFloatTy
            | ILCharTy
            | ILPtrTy
            | ILForgotTy
            deriving Eq

data ILInteger = ILBigInt Integer
               | ILBigIntExpr ILExpr
               deriving Eq

data ILNum = ILInt Int
           | ILFloat Double
           | ILInteger ILInteger
           deriving Eq

data ILWord = ILWord8 Word8
            | ILWord16 Word16
            | ILWord32 Word32
            | ILWord64 Word64
            deriving Eq

data ILAnnotation = ILConstructor deriving Eq

instance Show ILAnnotation where
  show ILConstructor = "class"

data ILExpr = ILRaw String
        | ILIdent String
        | ILFunction [String] ILExpr
        | ILType ILType
        | ILSeq [ILExpr]
        | ILList [ILExpr]
        | ILReturn ILExpr
        | ILApp ILExpr [ILExpr]
        | ILNew String [ILExpr]
        | ILError String
        | ILBinOp String ILExpr ILExpr
        | ILPreOp String ILExpr
        | ILPostOp String ILExpr
        | ILProj ILExpr String
        | ILPtrProj ILExpr String
        | ILArray [ILExpr]
        | ILString String
        | ILChar String
        | ILNum ILNum
        | ILWord ILWord
        | ILAssign ILExpr ILExpr
        | ILAlloc (Maybe String) String (Maybe ILExpr)
        | ILIndex ILExpr ILExpr
        | ILSwitch ILExpr [(ILExpr, ILExpr)] (Maybe ILExpr)
        | ILCond [(ILExpr, ILExpr)]
        | ILTernary ILExpr ILExpr ILExpr
        | ILParens ILExpr
        | ILWhile ILExpr ILExpr
        | ILFFI String [ILExpr]
        | ILAnnotation ILAnnotation ILExpr
        | ILNoop
        deriving Eq

data FFI = FFICode Char | FFIArg Int | FFIError String

ffi :: String -> [String] -> T.Text
ffi code args = let parsed = ffiParse code in
                    case ffiError parsed of
                         Just err -> error err
                         Nothing  -> if (any isPosArg parsed)
                                       then renderFFI parsed args
                                       else renderFFI (parsed ++ defArgSeq) args
  where
    ffiParse :: String -> [FFI]
    ffiParse ""           = []
    ffiParse ['%']        = [FFIError $ "FFI - Invalid positional argument"]
    ffiParse ('%':'%':ss) = FFICode '%' : ffiParse ss
    ffiParse ('%':s:ss)
      | isDigit s =
         FFIArg (
           read $ s : takeWhile isDigit ss
          ) : ffiParse (dropWhile isDigit ss)
      | otherwise =
          [FFIError "FFI - Invalid positional argument"]
    ffiParse (s:ss) = FFICode s : ffiParse ss

    isPosArg :: FFI -> Bool
    isPosArg x = case x of FFIArg _ -> True
                           _        -> False
    defArgSeq :: [FFI]
    defArgSeq = FFICode '(' : intersperse (FFICode ',') (map FFIArg (take (length args) [0..])) ++ [FFICode ')']

    ffiError :: [FFI] -> Maybe String
    ffiError []                 = Nothing
    ffiError ((FFIError s):xs)  = Just s
    ffiError (x:xs)             = ffiError xs


    renderFFI :: [FFI] -> [String] -> T.Text
    renderFFI [] _ = ""
    renderFFI (FFICode c : fs) args = c `T.cons` renderFFI fs args
    renderFFI (FFIArg i : fs) args
      | i < length args && i >= 0 =
            T.pack (args !! i)
          `T.append` renderFFI fs args
      | otherwise = error "FFI - Argument index out of bounds"

compileIL :: ILExpr -> T.Text
compileIL = compileIL' 0

compileIL' :: Int -> ILExpr -> T.Text
compileIL' indent ILNoop = ""

compileIL' indent (ILAnnotation annotation expr) =
   ""
  `T.append` T.pack (show annotation)
  `T.append` " */\n"
  `T.append` compileIL' indent expr

compileIL' indent (ILFFI raw args) =
  ffi raw (map (T.unpack . compileIL' indent) args)

compileIL' indent (ILRaw code) =
  T.pack code

compileIL' indent (ILIdent ident) =
  T.pack ident

compileIL' indent (ILFunction args body) =
   T.replicate indent " " `T.append` "("
   `T.append` T.intercalate "," (map T.pack args)
   `T.append` ") {\n"
   `T.append` compileIL' (indent + 2) body
   `T.append` "\n}\n"

compileIL' indent (ILSeq seq) =
  T.intercalate ";\n" (
    map (
      (T.replicate indent " " `T.append`) . (compileIL' indent)
    ) $ filter (/= ILNoop) seq
  ) `T.append` ";"

compileIL' indent (ILList seq) =
  T.intercalate "," (
    map (
      (T.replicate indent " " `T.append`) . (compileIL' indent)
    ) $ filter (/= ILNoop) seq
  )

compileIL' indent (ILReturn val) =
  "return " `T.append` compileIL' indent val

compileIL' indent (ILApp lhs rhs)
  | ILFunction {} <- lhs =
    T.concat ["(", compileIL' indent lhs, ")(", args, ")"]
  | otherwise =
    T.concat [compileIL' indent lhs, "(", args, ")"]
  where args :: T.Text
        args = T.intercalate "," $ map (compileIL' 0) rhs

compileIL' indent (ILNew name args) =
  T.pack name
  `T.append` "("
  `T.append` T.intercalate "," (map (compileIL' 0) args)
  `T.append` ")"

compileIL' indent (ILError exc) = compileIL (mkILCall "putStr" [ILString exc]) `T.append` "; assert(false)"

compileIL' indent (ILBinOp op lhs rhs) =
    compileIL' indent lhs
  `T.append` " "
  `T.append` T.pack op
  `T.append` " "
  `T.append` compileIL' indent rhs

compileIL' indent (ILPreOp op val) =
  T.pack op `T.append` compileIL' indent val

compileIL' indent (ILPostOp op val) =
  compileIL' indent val `T.append` T.pack op

compileIL' indent (ILProj obj field)
  | ILFunction {} <- obj =
    T.concat ["(", compileIL' indent obj, ").", T.pack field]
  | ILAssign {} <- obj =
    T.concat ["(", compileIL' indent obj, ").", T.pack field]
  | otherwise =
    compileIL' indent obj `T.append` "." `T.append` T.pack field

compileIL' indent (ILPtrProj obj field)
  | ILFunction {} <- obj =
    T.concat ["(", compileIL' indent obj, ")->", T.pack field]
  | ILAssign {} <- obj =
    T.concat ["(", compileIL' indent obj, ")->", T.pack field]
  | otherwise =
    compileIL' indent obj `T.append` "->" `T.append` T.pack field

compileIL' indent (ILArray elems) =
  "{" `T.append` T.intercalate "," (map (compileIL' 0) elems) `T.append` "}"

compileIL' indent (ILString str) =
  "\"" `T.append` T.pack str `T.append` "\""

compileIL' indent (ILChar c) =
  "'" `T.append` T.pack c `T.append` "'"

compileIL' indent (ILNum num) =
  case num of
    ILInt i                     -> T.pack (show i)
    ILFloat f                   -> T.pack (show f)
    ILInteger (ILBigInt i)     -> T.pack $ big i
    ILInteger (ILBigIntExpr e) -> compileIL' indent e
  where
    big :: Integer -> String
    big i
      | i > (toInteger (maxBound::Int)) || i < (toInteger (minBound::Int)) = "asBig(" ++ (show i) ++ ")"
      | otherwise = show i

compileIL' indent (ILAssign lhs rhs) =
  compileIL' indent lhs `T.append` " = " `T.append` compileIL' indent rhs

compileIL' 0 (ILAlloc _ name (Just val@(ILNew _ _))) =
  T.pack name
  `T.append` " = "
  `T.append` compileIL' 0 val
  `T.append` ";\n"

compileIL' indent (ILAlloc typename name val) =
    case val of Nothing   -> typ `T.append` T.pack name
                Just expr -> typ `T.append` T.pack name `T.append` " = " `T.append` compileIL' indent expr
                where
                  typ = case typename of Nothing -> T.pack "auto "
                                         Just t  -> T.pack (t ++ " ")
    -- let expr = maybe "" (compileIL' indent) val
    -- in case val of (Nothing)               -> ""
    --                (Just (ILFunction _ _)) -> (if name == "main" then "int " else "void ")
    --                                             `T.append` T.pack name `T.append`  expr
    --                (_)                       -> "auto " `T.append` T.pack name `T.append` " = " `T.append` expr

compileIL' indent (ILIndex lhs rhs) =
    compileIL' indent lhs
  `T.append` "["
  `T.append` compileIL' indent rhs
  `T.append` "]"

compileIL' indent (ILCond branches) =
  T.intercalate " else " $ map createIfBlock branches
  where
    createIfBlock (ILNoop, e@(ILSeq _)) =
         "{\n"
      `T.append` compileIL' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (ILNoop, e) =
         "{\n"
      `T.append` compileIL' (indent + 2) e
      `T.append` ";\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e@(ILSeq _)) =
         "if (" `T.append` compileIL' indent cond `T.append`") {\n"
      `T.append` compileIL' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e) =
         "if (" `T.append` compileIL' indent cond `T.append`") {\n"
      `T.append` T.replicate (indent + 2) " "
      `T.append` compileIL' (indent + 2) e
      `T.append` ";\n"
      `T.append` T.replicate indent " "
      `T.append` "}"

compileIL' indent (ILSwitch val [(_,ILSeq seq)] Nothing) =
  let (h,t) = splitAt 1 seq in
         (T.concat (map (compileIL' indent) h) `T.append` ";\n")
      `T.append` (
        T.intercalate ";\n" $ map (
          (T.replicate indent " " `T.append`) . compileIL' indent
        ) t
      )

compileIL' indent (ILSwitch val branches def) =
     "switch (" `T.append` compileIL' indent val `T.append` ") {\n"
  `T.append` T.concat (map mkBranch branches)
  `T.append` mkDefault def
  `T.append` T.replicate indent " " `T.append` "}"
  where
    mkBranch :: (ILExpr, ILExpr) -> T.Text
    mkBranch (tag, code) =
         T.replicate (indent + 2) " "
      `T.append` "case "
      `T.append` compileIL' indent tag
      `T.append` ":\n"
      `T.append` compileIL' (indent + 4) code
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"
      `T.append` (T.replicate (indent + 4) " " `T.append` "\n")

    mkDefault :: Maybe ILExpr -> T.Text
    mkDefault Nothing = ""
    mkDefault (Just def) =
         T.replicate (indent + 2) " " `T.append` "default:\n"
      `T.append` compileIL' (indent + 4)def
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"

compileIL' indent (ILTernary cond true false) =
  let c = compileIL' indent cond
      t = compileIL' indent true
      f = compileIL' indent false in
        "("
      `T.append` c
      `T.append` ")?("
      `T.append` t
      `T.append` "):("
      `T.append` f
      `T.append` ")"

compileIL' indent (ILParens expr) =
  "(" `T.append` compileIL' indent expr `T.append` ")"

compileIL' indent (ILWhile cond body) =
     "while (" `T.append` compileIL' indent cond `T.append` ") {\n"
  `T.append` compileIL' (indent + 2) body
  `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"

compileIL' indent (ILWord word)
  | ILWord8  b <- word = compileIL' indent (fromInt b)
  | ILWord16 b <- word = compileIL' indent (fromInt b)
  | ILWord32 b <- word = compileIL' indent (fromInt b)
  | ILWord64 b <- word = compileIL' indent (fromBigInt b)
    where
      fromInt n = ILNum $ ILInt (fromIntegral n)
      fromBigInt n = ILNum . ILInteger . ILBigInt $ fromIntegral n

mkILOr :: ILExpr -> ILExpr -> ILExpr
mkILOr lhs rhs = ILBinOp "||" lhs rhs

mkILAnd :: ILExpr -> ILExpr -> ILExpr
mkILAnd lhs rhs = ILBinOp "&&" lhs rhs

mkILMeth :: ILExpr -> String -> [ILExpr] -> ILExpr
mkILMeth obj meth args = ILApp (ILProj obj meth) args

mkILPtrMeth :: ILExpr -> String -> [ILExpr] -> ILExpr
mkILPtrMeth obj meth args = ILApp (ILPtrProj obj meth) args

mkILCall :: String -> [ILExpr] -> ILExpr
mkILCall fun args = ILApp (ILIdent fun) args

mkILEq :: ILExpr -> ILExpr -> ILExpr
mkILEq lhs rhs = ILBinOp "==" lhs rhs

mkILNotEq :: ILExpr -> ILExpr -> ILExpr
mkILNotEq lhs rhs = ILBinOp "!=" lhs rhs

mkILLessThan :: ILExpr -> ILExpr -> ILExpr
mkILLessThan lhs rhs = ILBinOp "<" lhs rhs

mkILLessThanEq :: ILExpr -> ILExpr -> ILExpr
mkILLessThanEq lhs rhs = ILBinOp "<=" lhs rhs

mkILGreaterThan :: ILExpr -> ILExpr -> ILExpr
mkILGreaterThan lhs rhs = ILBinOp ">" lhs rhs

mkILGreaterThanEq :: ILExpr -> ILExpr -> ILExpr
mkILGreaterThanEq lhs rhs = ILBinOp ">=" lhs rhs

mkILBitAnd :: ILExpr -> ILExpr -> ILExpr
mkILBitAnd lhs rhs = ILBinOp "&" lhs rhs

mkILBitOr :: ILExpr -> ILExpr -> ILExpr
mkILBitOr lhs rhs = ILBinOp "|" lhs rhs

mkILBitXor :: ILExpr -> ILExpr -> ILExpr
mkILBitXor lhs rhs = ILBinOp "^" lhs rhs

mkILBitShl :: ILExpr -> ILExpr -> ILExpr
mkILBitShl lhs rhs = ILBinOp "<<" lhs rhs

mkILBitShr :: ILExpr -> ILExpr -> ILExpr
mkILBitShr lhs rhs = ILBinOp ">>" lhs rhs

mkILBitCompl :: ILExpr -> ILExpr
mkILBitCompl n = ILPreOp "~" n

mkILZero :: ILExpr
mkILZero = ILNum (ILInt 0)

mkILOne :: ILExpr
mkILOne = ILNum (ILInt 1)

mkILAdd :: ILExpr -> ILExpr -> ILExpr
mkILAdd lhs rhs = ILBinOp "+" lhs rhs

mkILSubtract :: ILExpr -> ILExpr -> ILExpr
mkILSubtract lhs rhs = ILBinOp "-" lhs rhs

mkILMultiply :: ILExpr -> ILExpr -> ILExpr
mkILMultiply lhs rhs = ILBinOp "*" lhs rhs

mkILDivide :: ILExpr -> ILExpr -> ILExpr
mkILDivide lhs rhs = ILBinOp "/" lhs rhs

mkILModulo :: ILExpr -> ILExpr -> ILExpr
mkILModulo lhs rhs = ILBinOp "%" lhs rhs

