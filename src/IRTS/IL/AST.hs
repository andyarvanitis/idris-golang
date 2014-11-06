module IRTS.IL.AST where

import Data.Word

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

