{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module IRTS.Cpp.AST where

import Data.Word
import Data.Char (isDigit)
import Data.List (intersperse)
import Data.Int

import qualified Data.Text as T

data CppType = CppIntTy
            | CppStringTy
            | CppIntegerTy
            | CppFloatTy
            | CppCharTy
            | CppPtrTy
            | CppForgotTy
            deriving Eq

data CppInteger = CppBigInt Integer
               | CppBigIntExpr Cpp
               deriving Eq

data CppNum = CppInt Int
           | CppFloat Double
           | CppInteger CppInteger
           deriving Eq

data CppWord = CppWord8 Word8
            | CppWord16 Word16
            | CppWord32 Word32
            | CppWord64 Word64
            deriving Eq

data CppAnnotation = CppConstructor deriving Eq

instance Show CppAnnotation where
  show CppConstructor = "class"

data Cpp = CppRaw String
        | CppIdent String
        | CppFunction [String] Cpp
        | CppType CppType
        | CppSeq [Cpp]
        | CppList [Cpp]
        | CppReturn Cpp
        | CppApp Cpp [Cpp]
        | CppNew String [Cpp]
        | CppError String
        | CppBinOp String Cpp Cpp
        | CppPreOp String Cpp
        | CppPostOp String Cpp
        | CppProj Cpp String
        | CppPtrProj Cpp String
        | CppNull
        | CppUndefined
        | CppThis
        | CppTrue
        | CppFalse
        | CppArray [Cpp]
        | CppString String
        | CppChar String
        | CppNum CppNum
        | CppWord CppWord
        | CppAssign Cpp Cpp
        | CppAlloc (Maybe String) String (Maybe Cpp)
        | CppIndex Cpp Cpp
        | CppSwitch Cpp [(Cpp, Cpp)] (Maybe Cpp)
        | CppCond [(Cpp, Cpp)]
        | CppTernary Cpp Cpp Cpp
        | CppParens Cpp
        | CppWhile Cpp Cpp
        | CppFFI String [Cpp]
        | CppAnnotation CppAnnotation Cpp
        | CppNoop
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

compileCpp :: Cpp -> T.Text
compileCpp = compileCpp' 0

compileCpp' :: Int -> Cpp -> T.Text
compileCpp' indent CppNoop = ""

compileCpp' indent (CppAnnotation annotation cpp) =
   ""
  `T.append` T.pack (show annotation)
  `T.append` " */\n"
  `T.append` compileCpp' indent cpp

compileCpp' indent (CppFFI raw args) =
  ffi raw (map (T.unpack . compileCpp' indent) args)

compileCpp' indent (CppRaw code) =
  T.pack code

compileCpp' indent (CppIdent ident) =
  T.pack ident

compileCpp' indent (CppFunction args body) =
   T.replicate indent " " `T.append` "("
   `T.append` T.intercalate "," (map T.pack args)
   `T.append` ") {\n"
   `T.append` compileCpp' (indent + 2) body
   `T.append` "\n}\n"

compileCpp' indent (CppType ty)
  | CppIntTy     <- ty = "i_Int"
  | CppStringTy  <- ty = "i_String"
  | CppIntegerTy <- ty = "i_Integer"
  | CppFloatTy   <- ty = "i_Float"
  | CppCharTy    <- ty = "i_Char"
  | CppPtrTy     <- ty = "i_Ptr"
  | CppForgotTy  <- ty = "i_Forgot"

compileCpp' indent (CppSeq seq) =
  T.intercalate ";\n" (
    map (
      (T.replicate indent " " `T.append`) . (compileCpp' indent)
    ) $ filter (/= CppNoop) seq
  ) `T.append` ";"

compileCpp' indent (CppList seq) =
  T.intercalate "," (
    map (
      (T.replicate indent " " `T.append`) . (compileCpp' indent)
    ) $ filter (/= CppNoop) seq
  )

compileCpp' indent (CppReturn val) =
  "return " `T.append` compileCpp' indent val

compileCpp' indent (CppApp lhs rhs)
  | CppFunction {} <- lhs =
    T.concat ["(", compileCpp' indent lhs, ")(", args, ")"]
  | otherwise =
    T.concat [compileCpp' indent lhs, "(", args, ")"]
  where args :: T.Text
        args = T.intercalate "," $ map (compileCpp' 0) rhs

compileCpp' indent (CppNew name args) =
  T.pack name
  `T.append` "("
  `T.append` T.intercalate "," (map (compileCpp' 0) args)
  `T.append` ")"

compileCpp' indent (CppError exc) = compileCpp (cppCall "putStr" [CppString exc]) `T.append` "; assert(false)"

compileCpp' indent (CppBinOp op lhs rhs) =
    compileCpp' indent lhs
  `T.append` " "
  `T.append` T.pack op
  `T.append` " "
  `T.append` compileCpp' indent rhs

compileCpp' indent (CppPreOp op val) =
  T.pack op `T.append` compileCpp' indent val

compileCpp' indent (CppPostOp op val) =
  compileCpp' indent val `T.append` T.pack op

compileCpp' indent (CppProj obj field)
  | CppFunction {} <- obj =
    T.concat ["(", compileCpp' indent obj, ").", T.pack field]
  | CppAssign {} <- obj =
    T.concat ["(", compileCpp' indent obj, ").", T.pack field]
  | otherwise =
    compileCpp' indent obj `T.append` "." `T.append` T.pack field

compileCpp' indent (CppPtrProj obj field)
  | CppFunction {} <- obj =
    T.concat ["(", compileCpp' indent obj, ")->", T.pack field]
  | CppAssign {} <- obj =
    T.concat ["(", compileCpp' indent obj, ")->", T.pack field]
  | otherwise =
    compileCpp' indent obj `T.append` "->" `T.append` T.pack field

compileCpp' indent CppNull =
  "nullptr"

compileCpp' indent CppUndefined =
  "undefined"

compileCpp' indent CppThis =
  "this"

compileCpp' indent CppTrue =
  "true"

compileCpp' indent CppFalse =
  "false"

compileCpp' indent (CppArray elems) =
  "{" `T.append` T.intercalate "," (map (compileCpp' 0) elems) `T.append` "}"

compileCpp' indent (CppString str) =
  "\"" `T.append` T.pack str `T.append` "\""

compileCpp' indent (CppChar c) =
  "'" `T.append` T.pack c `T.append` "'"

compileCpp' indent (CppNum num) =
  case num of
    CppInt i                     -> T.pack (show i)
    CppFloat f                   -> T.pack (show f)
    CppInteger (CppBigInt i)     -> T.pack $ big i
    CppInteger (CppBigIntExpr e) -> compileCpp' indent e
  where
    big :: Integer -> String
    big i
      | i > (toInteger (maxBound::Int)) || i < (toInteger (minBound::Int)) = "asBig(" ++ (show i) ++ ")"
      | otherwise = show i

compileCpp' indent (CppAssign lhs rhs) =
  compileCpp' indent lhs `T.append` " = " `T.append` compileCpp' indent rhs

compileCpp' 0 (CppAlloc _ name (Just val@(CppNew _ _))) =
  T.pack name
  `T.append` " = "
  `T.append` compileCpp' 0 val
  `T.append` ";\n"

compileCpp' indent (CppAlloc typename name val) =
    case val of Nothing   -> typ `T.append` T.pack name
                Just expr -> typ `T.append` T.pack name `T.append` " = " `T.append` compileCpp' indent expr
                where
                  typ = case typename of Nothing -> T.pack "auto "
                                         Just t  -> T.pack (t ++ " ")
    -- let expr = maybe "" (compileCpp' indent) val
    -- in case val of (Nothing)               -> ""
    --                (Just (CppFunction _ _)) -> (if name == "main" then "int " else "void ")
    --                                             `T.append` T.pack name `T.append`  expr
    --                (_)                       -> "auto " `T.append` T.pack name `T.append` " = " `T.append` expr

compileCpp' indent (CppIndex lhs rhs) =
    compileCpp' indent lhs
  `T.append` "["
  `T.append` compileCpp' indent rhs
  `T.append` "]"

compileCpp' indent (CppCond branches) =
  T.intercalate " else " $ map createIfBlock branches
  where
    createIfBlock (CppNoop, e@(CppSeq _)) =
         "{\n"
      `T.append` compileCpp' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (CppNoop, e) =
         "{\n"
      `T.append` compileCpp' (indent + 2) e
      `T.append` ";\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e@(CppSeq _)) =
         "if (" `T.append` compileCpp' indent cond `T.append`") {\n"
      `T.append` compileCpp' (indent + 2) e
      `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"
    createIfBlock (cond, e) =
         "if (" `T.append` compileCpp' indent cond `T.append`") {\n"
      `T.append` T.replicate (indent + 2) " "
      `T.append` compileCpp' (indent + 2) e
      `T.append` ";\n"
      `T.append` T.replicate indent " "
      `T.append` "}"

compileCpp' indent (CppSwitch val [(_,CppSeq seq)] Nothing) =
  let (h,t) = splitAt 1 seq in
         (T.concat (map (compileCpp' indent) h) `T.append` ";\n")
      `T.append` (
        T.intercalate ";\n" $ map (
          (T.replicate indent " " `T.append`) . compileCpp' indent
        ) t
      )

compileCpp' indent (CppSwitch val branches def) =
     "switch (" `T.append` compileCpp' indent val `T.append` ") {\n"
  `T.append` T.concat (map mkBranch branches)
  `T.append` mkDefault def
  `T.append` T.replicate indent " " `T.append` "}"
  where
    mkBranch :: (Cpp, Cpp) -> T.Text
    mkBranch (tag, code) =
         T.replicate (indent + 2) " "
      `T.append` "case "
      `T.append` compileCpp' indent tag
      `T.append` ":\n"
      `T.append` compileCpp' (indent + 4) code
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"
      `T.append` (T.replicate (indent + 4) " " `T.append` "\n")

    mkDefault :: Maybe Cpp -> T.Text
    mkDefault Nothing = ""
    mkDefault (Just def) =
         T.replicate (indent + 2) " " `T.append` "default:\n"
      `T.append` compileCpp' (indent + 4)def
      `T.append` "\n" `T.append` T.replicate (indent + 2) " " `T.append` "break;\n"

compileCpp' indent (CppTernary cond true false) =
  let c = compileCpp' indent cond
      t = compileCpp' indent true
      f = compileCpp' indent false in
        "("
      `T.append` c
      `T.append` ")?("
      `T.append` t
      `T.append` "):("
      `T.append` f
      `T.append` ")"

compileCpp' indent (CppParens cpp) =
  "(" `T.append` compileCpp' indent cpp `T.append` ")"

compileCpp' indent (CppWhile cond body) =
     "while (" `T.append` compileCpp' indent cond `T.append` ") {\n"
  `T.append` compileCpp' (indent + 2) body
  `T.append` "\n" `T.append` T.replicate indent " " `T.append` "}"

compileCpp' indent (CppWord word)
  | CppWord8  b <- word = compileCpp' indent (fromInt b)
  | CppWord16 b <- word = compileCpp' indent (fromInt b)
  | CppWord32 b <- word = compileCpp' indent (fromInt b)
  | CppWord64 b <- word = compileCpp' indent (fromBigInt b)
    where
      fromInt n = CppNum $ CppInt (fromIntegral n)
      fromBigInt n = CppNum . CppInteger . CppBigInt $ fromIntegral n

cppInstanceOf :: Cpp -> String -> Cpp
cppInstanceOf obj cls = cppAnd obj (cppEq (cppPtrMeth obj "getTypeId" []) (CppChar cls))

cppOr :: Cpp -> Cpp -> Cpp
cppOr lhs rhs = CppBinOp "||" lhs rhs

cppAnd :: Cpp -> Cpp -> Cpp
cppAnd lhs rhs = CppBinOp "&&" lhs rhs

cppMeth :: Cpp -> String -> [Cpp] -> Cpp
cppMeth obj meth args = CppApp (CppProj obj meth) args

cppPtrMeth :: Cpp -> String -> [Cpp] -> Cpp
cppPtrMeth obj meth args = CppApp (CppPtrProj obj meth) args

cppCall :: String -> [Cpp] -> Cpp
cppCall fun args = CppApp (CppIdent fun) args

cppEq :: Cpp -> Cpp -> Cpp
cppEq lhs rhs = CppBinOp "==" lhs rhs

cppNotEq :: Cpp -> Cpp -> Cpp
cppNotEq lhs rhs = CppBinOp "!=" lhs rhs

cppGreaterThan :: Cpp -> Cpp -> Cpp
cppGreaterThan lhs rhs = CppBinOp ">" lhs rhs

cppZero :: Cpp
cppZero = CppNum (CppInt 0)

cppOne :: Cpp
cppOne = CppNum (CppInt 1)

cppIsNull :: Cpp -> Cpp
cppIsNull cpp = CppBinOp "==" cpp CppNull

cppIsNotNull :: Cpp -> Cpp
cppIsNotNull cpp = CppBinOp "!=" cpp CppNull

cppStaticCast :: String -> Cpp -> Cpp
cppStaticCast typ cpp = cppCall ("static_cast" ++ "<" ++ typ ++ ">") [cpp]
