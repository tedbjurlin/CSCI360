-- Class 02/17

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Char (isSpace, isDigit)

data Op where
    Plus  :: Op
    Minus :: Op
    Times :: Op
    deriving (Show, Eq)

data Arith where
    Lit :: Integer -> Arith
    Bin :: Op -> Arith -> Arith -> Arith
    deriving (Show)

--------------------------------------------------------------------------------

-- Step 1: Tokenizer

data Token where
    TLit :: Integer -> Token
    TLParen :: Token
    TRParen :: Token
    TOp :: Op -> Token
    deriving Show

tokenize :: String -> [Token]
tokenize ""         = []
tokenize (c : cs)
    | isSpace c = tokenize cs
tokenize ('(' : cs) = TLParen : tokenize cs
tokenize (')' : cs) = TRParen : tokenize cs
tokenize ('+' : cs) = TOp Plus : tokenize cs
tokenize ('-' : cs) = TOp Minus : tokenize cs
tokenize ('*' : cs) = TOp Times : tokenize cs
tokenize (d : cs)
    | isDigit d = case span isDigit (d:cs) of
        (digits, rest) -> TLit (read digits) : tokenize rest
tokenize (c: _) = error("Invalid Character: " ++ [c])

--------------------------------------------------------------------------------

-- Step 2: Shunting Yard
--      Convert tokens from infix to postfix notations

type Precedence = Int

prec :: Token -> Precedence
prec TRParen     = 0
prec TLParen     = 0 
prec (TOp Plus)  = 1
prec (TOp Minus) = 1
prec (TOp Times) = 2
prec (TLit _)    = 3

shunt :: [Token] -> [Token]
shunt = shuntSupreme []

shuntSupreme :: [Token] -> [Token] -> [Token]
shuntSupreme stk [] = stk
shuntSupreme stk (TLit n : ts)              = TLit n : shuntSupreme stk ts
shuntSupreme stk (TLParen : ts)             = shuntSupreme (TLParen : stk) ts
shuntSupreme (TLParen : stk) (TRParen : ts) = shuntSupreme stk ts
shuntSupreme (op : stk) (TRParen : ts)      = op : shuntSupreme stk (TRParen : ts)
shuntSupreme [] (TOp op : ts)               = shuntSupreme [TOp op] ts
shuntSupreme (topOp : stk) (TOp op : ts)
    | prec topOp >= prec (TOp op) = topOp : shuntSupreme stk (TOp op : ts)
    | otherwise = shuntSupreme (TOp op : topOp : stk) ts
shuntSupreme _ _ = error ""