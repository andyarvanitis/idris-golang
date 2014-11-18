{-# LANGUAGE OverloadedStrings #-}

module IRTS.Generic.CodegenFFI where

import Data.Char
import Data.List (intersperse)

import qualified Data.Text as T
import qualified Text.Printf as PF

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

