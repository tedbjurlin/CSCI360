{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTSyntax #-}

module Intercal where

import qualified Data.Map as M
import           Text.Megaparsec

---------------------------------- Abstract Syntax Tree ----------------------------------

type Prog = [Stmt]

data UOp where
    And :: UOp
    Or  :: UOp
    XOR :: UOp
    deriving Show

data BOp where
    Ilv :: BOp
    Sel :: BOp
    deriving Show

data Exp where
    Array16 :: Integer -> Exp
    Array32 :: Integer -> Exp
    Var16   :: Integer -> Exp
    Var32   :: Integer -> Exp
    Const   :: Integer -> Exp
    Sub     :: Exp -> [Exp] -> Exp
    Una     :: UOp -> Exp -> Exp
    Bin     :: BOp -> Exp -> Exp -> Exp
    deriving Show

data Quantifier where
    Not     :: Quantifier
    Percent :: Integer -> Quantifier
    deriving Show

data Gerund = Forgetting | Resuming | Stashing | Retrieving | Ignoring | Remebering | Abstaining | Reinstating
    deriving Show

data StmtOp where
    Calc      :: Exp -> Exp -> StmtOp
    CalcDim   :: Exp -> [Exp] -> StmtOp
    Next      :: Integer -> StmtOp
    Forget    :: Exp -> StmtOp
    Resume    :: Exp -> StmtOp
    Stash     :: [Exp] -> StmtOp
    Retrieve  :: [Exp] -> StmtOp
    Ignore    :: [Exp] -> StmtOp
    Remeber   :: [Exp] -> StmtOp
    Abstain   :: [Gerund] -> StmtOp
    Reinstate :: [Gerund] -> StmtOp
    GiveUp    :: StmtOp
    deriving Show

data Stmt where
    Stmt :: Maybe Integer -> Maybe Quantifier -> StmtOp -> Stmt
    Cmnt :: String -> Stmt
    deriving Show

----------------------------------------- Parser -----------------------------------------

