module IRTS.Generic.AST where

import Data.Word

data ASTType = ASTIntTy
            | ASTStringTy
            | ASTIntegerTy
            | ASTFloatTy
            | ASTCharTy
            | ASTPtrTy
            | ASTForgotTy
            deriving Eq

data ASTInteger = ASTBigInt Integer
               | ASTBigIntExpr ASTNode
               deriving Eq

data ASTNum = ASTInt Int
           | ASTFloat Double
           | ASTInteger ASTInteger
           deriving Eq

data ASTWord = ASTWord8 Word8
            | ASTWord16 Word16
            | ASTWord32 Word32
            | ASTWord64 Word64
            deriving Eq

data ASTAnnotation = ASTConstructor deriving Eq

instance Show ASTAnnotation where
  show ASTConstructor = "class"

data ASTNode = ASTRaw String
        | ASTIdent String
        | ASTFunction [String] ASTNode
        | ASTType ASTType
        | ASTSeq [ASTNode]
        | ASTList [ASTNode]
        | ASTReturn ASTNode
        | ASTApp ASTNode [ASTNode]
        | ASTError String
        | ASTBinOp String ASTNode ASTNode
        | ASTPreOp String ASTNode
        | ASTPostOp String ASTNode
        | ASTProj ASTNode String
        | ASTPtrProj ASTNode String
        | ASTArray [ASTNode]
        | ASTString String
        | ASTChar String
        | ASTNum ASTNum
        | ASTWord ASTWord
        | ASTAssign ASTNode ASTNode
        | ASTAlloc (Maybe String) String (Maybe ASTNode)
        | ASTIndex ASTNode ASTNode
        | ASTSwitch ASTNode [(ASTNode, ASTNode)] (Maybe ASTNode)
        | ASTCond [(ASTNode, ASTNode)]
        | ASTTernary ASTNode ASTNode ASTNode
        | ASTParens ASTNode
        | ASTWhile ASTNode ASTNode
        | ASTFFI String [ASTNode]
        | ASTAnnotation ASTAnnotation ASTNode
        | ASTNoop
        deriving Eq

