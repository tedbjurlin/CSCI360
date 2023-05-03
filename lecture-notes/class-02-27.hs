-- Example:
-- write a parser which parses two integers in parentheses
-- with a  + in between them, and returns the sum.

{-# LANGUAGE GADTs #-}

import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))
import Parsing

lexer :: TokenParser u
lexer = makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = getParens lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

integer :: Parser Integer
integer = getInteger lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

-- f <$> parser1 <*> parser2 <*> parser3 <*> ...
-- this pattern runs all the parsers and then combines them with
-- the results of the function f.

-- f :: a -> b -> c -> d
-- parser1 :: Parser a
-- parser2 :: Parser b
-- parser3 :: Parser c

-- (f <$> parser1 <*> parser2 <*> parser3) :: Parser d

norbert :: Parser Integer
norbert = parens ((+) <$> integer <*> (reservedOp "+" *> integer))