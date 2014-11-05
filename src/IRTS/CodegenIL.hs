module IRTS.CodegenIL where

import Idris.AbsSyntax hiding (TypeCase)
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.System hiding (getDataDir)
import IRTS.CodegenCommon
import IRTS.IL.AST
import Idris.Core.TT
-- import Util.System

import Numeric
import Data.Char
import Data.List (intercalate)
-- import System.Process
-- import System.Exit
-- import System.IO
-- import System.Directory
-- import Control.Monad.State
-- import Control.Arrow

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Printf as PF

class CompileInfo a where
  mkAssign :: a -> Reg -> Reg -> ILExpr
  mkAssignConst :: a -> Reg -> Const -> ILExpr
  mkAddTop :: a -> Int -> ILExpr
  mkNull :: a -> Reg -> ILExpr
  mkCall :: a -> Name -> ILExpr
  mkTailCall :: a -> Name -> ILExpr
  mkForeign :: a -> Reg -> String -> [(FType, Reg)] -> FType -> ILExpr
  mkTopBase :: a -> Int -> ILExpr
  mkBaseTop :: a -> Int -> ILExpr
  mkStoreOld :: a -> ILExpr


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

mkILCodepoint :: Int -> String
mkILCodepoint c = PF.printf "\\U%.8X" c


translateName :: Name -> String
translateName n = "_idris_" ++ concatMap cchar (showCG n)
  where cchar x | isAlphaNum x = [x]
                | otherwise    = "_" ++ show (fromEnum x) ++ "_"


translateBC :: CompileInfo a => a -> BC -> ILExpr
translateBC info bc =
  case bc of
    ASSIGN r1 r2          ->  mkAssign info r1 r2
    ASSIGNCONST r c       ->  mkAssignConst info r c
    UPDATE r1 r2          ->  mkAssign info r1 r2
    ADDTOP n              ->  mkAddTop info n
    NULL r                ->  mkNull info r
    CALL n                ->  mkCall info n
    TAILCALL n            ->  mkTailCall info n
    FOREIGNCALL r _ t n a ->  mkForeign info r n a t
    TOPBASE n             ->  mkTopBase info n
    BASETOP n             ->  mkBaseTop info n
    STOREOLD              ->  mkStoreOld info
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
    
