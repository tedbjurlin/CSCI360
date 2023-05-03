-- CSCI 360, Spring 2023
-- Project 3: the Quilt language

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}

{-# LANGUAGE GADTSyntax #-}

module Quilt where

import qualified Data.Map as M
import           Parsing2

----------------------------- Abstract Syntax Tree -----------------------------
{- | The `Coord` adt represents a reference to the x or y coordinate in the `QExp`

<coord> ::= 'x' | 'y'
-}
data Coord = X | Y
    deriving Show

{- | I have selected the colors of the rainbow for the color literals,
as well as white black and grey.

<colorlit> ::= 'red' | 'orange' | 'yellow' | ...
-}
data CLit = Black | White | Grey | Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving Show

{- | `UOp` represents the unary operators for negation and boolean inversion

<uop> ::= '-' | '!'
-}
data UOp = Neg | Not
    deriving Show

{- | `BOp` represents three types of binary operators: arithmetic, comparitive,
and boolean operators.

<bop> ::= <arith> | <comparison> | <boolean>
-}
data BOp where
    ABOp :: Arith -> BOp    -- ^ `ABOp` takes an arithmetic operator
    BBOp :: BoolOp -> BOp   -- ^ `BBOp` takes a boolean operator
    CBOp :: Compare -> BOp  -- ^ `CBOp` takes a comparative operator
    deriving Show

{- | `Arith` represents the arithmetic operators, which take two `Double`s and
return a `Double`

<arith> ::= '+' | '-' | '*' | '/'
-}
data Arith = Add | Sub | Mul | Div
    deriving Show

{- | `Compare` operators compare two `Double`s and return a `Bool`

<comparison> ::= '<' | '>' | '==' | '!=' | '<=' | '>='
-}
data Compare = Eq | NotEq | Great | Less | GreatEq | LessEq
    deriving Show

{- | `BoolOp` operators take two `Bool`s and return a `Bool`

<boolean> ::= '&&' | '||'
-}
data BoolOp = And | Or
    deriving Show

{- | `QExp` is the main AST for the quilt language.
I went with `Let` expressions as my extension for level 2. They are good bang for the
buck, as a relatively simple extension that allows for much more complex quilts

<qexp> ::=
  | <colorlit>
  | <num>
  | <coord>
  | <bool>
  | '[' <qexp> ',' <qexp> ',' <qexp> ']'
  | 'if' <qexp> 'then' <qexp> 'else' <qexp>
  | <uop> <qexp>
  | <qexp> <bop> <qexp>
  | 'quilt' <qexp> <qexp> <qexp> <qexp>
  | 'let' <var> = <qexp> in <qexp>
-}
data QExp where
    -- | `ClrLit` through `CoordLit` represent their corresponding ADTs in `QExp`
    ClrLit   :: CLit -> QExp
    Num      :: Double -> QExp
    BoolLit  :: Bool -> QExp
    CoordLit :: Coord -> QExp
    -- | A `ClrExp` is a list of three `QExp`s that each correspond to a color channel
    ClrExp   :: QExp -> QExp -> QExp -> QExp
    -- | An `If` choses either the second or third `QExp` depending on the first
    If       :: QExp -> QExp -> QExp -> QExp
    -- | `Una` combines a `UOp` with the expression it acts on
    Una      :: UOp -> QExp -> QExp
    -- | Similarly, `Bin` takes a `BOp` and the first and second `QExp`s it acts on
    Bin      :: BOp -> QExp -> QExp -> QExp
    -- | `Quilt` splits the image into four quadrants and displays each `QExp` in one
    Quilt    :: QExp -> QExp -> QExp -> QExp -> QExp
    -- | `Var` stores the name of a referneced variable
    Var      :: String -> QExp
    -- | `Let` creates a variable that can be refernced in the second expression
    Let      :: String -> QExp -> QExp -> QExp
    deriving Show

------------------------------------ Parser ------------------------------------

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef
    { reservedNames =
        [ "if", "then", "else", "quilt", "True", "False", "white", "black"
        , "grey", "red", "orange", "yellow", "green", "blue", "indigo", "violet"
        , "let", "in" ]
    , reservedOpNames =
        [ "!", "-", "*", "/", "+", ">", "<", "="
        , ">=", "<=", "==", "!=", "&&", "||" ] }

parens :: Parser a -> Parser a
parens = getParens lexer

reserved :: String -> Parser ()
reserved = getReservedOp lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = getNaturalOrFloat lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

identifier :: Parser String
identifier = getIdentifier lexer

parseQExp :: Parser QExp
parseQExp = buildExpressionParser table parseQExpAtom
  where
    table = [ [ Prefix (Una Not <$ reservedOp "!")
              ]
            , [ Prefix (Una Neg <$ reservedOp "-")
              ]
            , [ Infix (Bin (ABOp Mul) <$ reservedOp "*") AssocLeft
              , Infix (Bin (ABOp Div) <$ reservedOp "/") AssocLeft
              ]
            , [ Infix (Bin (ABOp Add) <$ reservedOp "+") AssocLeft
              , Infix (Bin (ABOp Sub) <$ reservedOp "-") AssocLeft
              ]
            , [ Infix (Bin (CBOp Great) <$ reservedOp ">") AssocLeft
              , Infix (Bin (CBOp Less) <$ reservedOp "<") AssocLeft
              , Infix (Bin (CBOp GreatEq) <$ reservedOp ">=") AssocLeft
              , Infix (Bin (CBOp LessEq) <$ reservedOp "<=") AssocLeft
              ]
            , [ Infix (Bin (CBOp Eq) <$ reservedOp "==") AssocLeft
              , Infix (Bin (CBOp NotEq) <$ reservedOp "!=") AssocLeft
              ]
            , [ Infix (Bin (BBOp And) <$ reservedOp "&&") AssocLeft
              ]
            , [ Infix (Bin (BBOp Or) <$ reservedOp "||") AssocLeft
              ]
            ]

-- | To convert the output from `naturalOrFloat` to `Double`
eitherABToB :: (a -> b) -> Either a b -> b
eitherABToB f (Left a)  = f a
eitherABToB _ (Right b) = b

parseQExpAtom :: Parser QExp
parseQExpAtom
    = parseColorLit
    <|> (Num <$> (eitherABToB fromIntegral <$> naturalOrFloat))
    <|> (CoordLit X <$ symbol "x")
    <|> (CoordLit Y <$ symbol "y")
    <|> (BoolLit True <$ reserved "True")
    <|> (BoolLit False <$ reserved "False")
    <|> parens parseQExp
    <|> (symbol "[" *> (ClrExp
        <$> (parseQExp <* symbol ",")
        <*> (parseQExp <* symbol ",")
        <*> parseQExp) <* symbol "]")
    <|> (If
        <$> (reserved "if" *> parseQExp)
        <*> (reserved "then" *> parseQExp)
        <*> (reserved "else" *> parseQExp))
    <|> (Quilt
        <$> (reserved "quilt" *> parseQExp)
        <*> parseQExp
        <*> parseQExp
        <*> parseQExp)
    <|> (Var <$> identifier)
    <|> parseLet

parseLet :: Parser QExp
parseLet = Let <$> (reserved "let" *> identifier <* reservedOp "=") <*> (parseQExp <* reserved "in") <*> parseQExp

parseColorLit :: Parser QExp
parseColorLit
    = (ClrLit White <$ reserved "white")
    <|> (ClrLit Black <$ reserved "black")
    <|> (ClrLit Red <$ reserved "red")
    <|> (ClrLit Orange <$ reserved "orange")
    <|> (ClrLit Yellow <$ reserved "yellow")
    <|> (ClrLit Green <$ reserved "green")
    <|> (ClrLit Blue <$ reserved "blue")
    <|> (ClrLit Indigo <$ reserved "indigo")
    <|> (ClrLit Violet <$ reserved "violet")
    <|> (ClrLit Grey <$ reserved "grey")

quilt :: Parser QExp
quilt = whiteSpace *> parseQExp <* eof

--------------------------------- Type Checking --------------------------------

data TypeError where
    {- | root `QExp` -> problem `QExp` -> Expected type -> Actual type
    `Mismatch` is used when a type encountered is not the expected type
    -}
    Mismatch :: QExp -> QExp -> Type -> Type -> TypeError
    {- | root `QExp` -> type 1 -> type 2
    `Incompat` is used to indicate two types not being compatible (meaning one is a subtype
    of the other)
    -}
    Incompat :: QExp -> Type -> Type -> TypeError
    {- | variable name
    `Undef` is used when a variable used before it is defined
    -}
    Undef    :: String -> TypeError
    deriving Show

{- | These are the three types that a `QExp` can have. A `Number` is a `Double`,
a `Boolean` is a `Bool` and a `Color` is a list of three `Double`s
-}
data Type = Boolean | Number | Color
    deriving (Show, Eq)

type Ctx = M.Map String Type

{- | `isSubtype` identifies if a type is a subtype of another type. `Number` is a
subtype of `Color`, and all types are subtypes of themselves. Notably, it can also
be used to test for equality between types, while keeping into account subtyping
-}
isSubtype :: Type -> Type -> Bool
isSubtype Number Color    = True
isSubtype t1 t2           = t1 == t2

{- | `compatOut` will either return the lowest type that both types are subtypes of
or throw an `Incompat` if neither is a subtype of the other. It takes the root `QExp`
to display in the error message
-}
compatOut :: Type -> Type -> QExp -> Either TypeError Type
compatOut t1 t2 root
  | isSubtype t1 t2 = Right t2
  | isSubtype t2 t1 = Right t1
  | otherwise       = Left (Incompat root t1 t2)

{- | `compatInfer` `infer`s the type of two `QExp`s and then uses calls `compatOut` on
them. It takes a third root `QExp` for error messages
-}
compatInfer :: Ctx -> QExp -> QExp -> QExp-> Either TypeError Type
compatInfer c e1 e2 root = infer c e1
    >>= \t1 -> infer c e2
    >>= \t2 -> compatOut t1 t2 root

{- | `check` `infer`s the type of a `QExp` and uses `isSubtype` to test if it is
equivalent to the expected `Type`
-}
check :: Ctx -> QExp -> Type -> QExp -> Either TypeError Type
check c e1 t1 root = infer c e1>>= \t2 -> 
    if isSubtype t2 t1
    then Right t2
    else Left (Mismatch root e1 t1 t2)

-- | `uopType` and `bopType` give the expected input types of the unary and binary operators
bopType :: BOp -> Type
bopType (ABOp _) = Color
bopType (BBOp _) = Boolean
bopType (CBOp _) = Number

uopType :: UOp -> Type
uopType Neg = Color
uopType Not = Boolean

{- | The `infer` function takes care if identifying the type of every a `QExp`. It
also takes a `Ctx`, which keeps track of the types of any variables encountered
-}
infer :: Ctx -> QExp -> Either TypeError Type
-- | `CLrLit` through `CoordLit` simply return the expected types of their literals
infer _ (ClrLit _)   = Right Color
infer _ (Num _)      = Right Number
infer _ (BoolLit _)  = Right Boolean
infer _ (CoordLit _) = Right Number
-- | Unary operators are simple to handle: check the expression against the operator type
infer c (Una op e)   = check c e (uopType op) (Una op e)
-- | For `ClrExp` we check that all the expressions are `Number`s and return `Color`
infer c (ClrExp e1 e2 e3)     
    = check c e1 Number (ClrExp e1 e2 e3)
    *> check c e2 Number (ClrExp e1 e2 e3)
    *> check c e3 Number (ClrExp e1 e2 e3)
    *> Right Color
-- | For an `If` we ckeck that the e1 is `Boolean` and return a compatible type from e2 and e2
infer c (If e1 e2 e3)
    = check c e1 Boolean (If e1 e2 e3) 
    *> compatInfer c e2 e3 (If e1 e2 e3)
-- | Comparitive binary operators have to be checked for the right input, and return `Boolean`
infer c (Bin (CBOp op) e1 e2)
    = check c e1 (bopType (CBOp op)) (Bin (CBOp op) e1 e2)
    >>= \t1 -> check c e2 (bopType (CBOp op)) (Bin (CBOp op) e1 e2)
    >>= \t2 -> compatOut t1 t2 (Bin (CBOp op) e1 e2)
    *> Right Boolean
-- | All other binary operators return the same type as their input types
infer c (Bin op e1 e2)
    = check c e1 (bopType op) (Bin op e1 e2)
    >>= \t1 -> check c e2 (bopType op) (Bin op e1 e2)
    >>= \t2 -> compatOut t1 t2 (Bin op e1 e2)
    >>= \tout -> Right tout
-- | `Quilt`s need to ensure that all four expressions have the same type, and return that type
infer c (Quilt e1 e2 e3 e4)
    = compatInfer c e1 e2 (Quilt e1 e2 e3 e4)
    >>= \t1 -> compatInfer c e3 e4 (Quilt e1 e2 e3 e4)
    >>= \t2 -> compatOut t1 t2 (Quilt e1 e2 e3 e4)
    >>= \tout -> Right tout
-- | `Var` expressions lookup the type of the var, and raise an error if it is not found
infer c (Var s) = case M.lookup s c of
    Just t  -> Right t
    Nothing -> Left (Undef s)
-- | `Let` expressions infer the type of e2 using the type of e1 as a variable
infer c (Let s e1 e2) = infer c e1 >>= \t1 -> infer (M.insert s t1 c) e2 

-- | A function to return the type of a parsed quilt expression
testTypecheck :: String -> String
testTypecheck s = case parse quilt s of
    Left err -> show err
    Right q  -> case infer M.empty q of
        Left err -> show err
        Right t  -> show t

---------------------------------- Interpreter ---------------------------------

-- | The envrionment maps variable names to `QuiltFun`s for use in `Let` expressions
type Env = M.Map String QuiltFun

-- | Convers a `Bool` to a `Color` representation
fromBoolean :: Bool -> Color
fromBoolean True  = [1.0, 1.0, 1.0]
fromBoolean False = [0.0, 0.0, 0.0]

{- | Converts a `Color` to a `Bool`. The context that this function is used ensures
that it will only recieve bool-like `Color`s
-}
toBoolean :: Color -> Bool
toBoolean [1.0, 1.0, 1.0] = True
toBoolean [0.0, 0.0, 0.0] = False
toBoolean _               = error "Unreachable error. Typechecking should prevent non-boolean colors."

{- | The `interpQuilt` function interprets `QExp` expressions into `QuiltFun` functions
which relate a pair of coordinates to a `Color`. All types are represented as `Color`s
-}
interpQuilt :: Env -> QExp -> QuiltFun
-- | `Number`s are represented as a color with all three values as the number
interpQuilt _ (Num n)             = \_ _ -> [n, n, n]
-- | `Boolean`s are represented with 1 for True and 0 for False
interpQuilt _ (BoolLit b)         = \_ _ -> fromBoolean b
interpQuilt _ (ClrLit cl)         = \_ _ -> interpClrLit cl
-- | `X` has all values as x
interpQuilt _ (CoordLit X)        = \x _ -> [x, x, x]
-- | `Y` has all values as y
interpQuilt _ (CoordLit Y)        = \_ y -> [y, y, y]
-- | `ClrExp`s have all three values interpreted, taking the first value of each as n
interpQuilt env (ClrExp e1 e2 e3) = \x y ->
    case (interpQuilt env e1 x y, interpQuilt env e2 x y, interpQuilt env e3 x y) of
        ([r,_,_], [g,_,_], [b,_,_]) -> [r, g, b]
        (_, _, _)          -> error 
            "Unreachable error. Color expressions should always have length 3"
-- | `If` statements only interpret the branch that gets chosen by the boolean
interpQuilt env (If b1 e1 e2) = \x y -> case interpQuilt env b1 x y of
    [1,1,1] -> interpQuilt env e1 x y
    [0,0,0] -> interpQuilt env e2 x y
    _   -> error
        "Unreachable error. Typechecking should insure that Boolean has length 3."
-- | Unary negation negates the first value and places it as r, g, and b
interpQuilt env (Una Neg e) = \x y -> case interpQuilt env e x y of
    [n,_,_] -> [-n, -n, -n]
    _       -> error
        "Unreachable error. Typechecking should ensure Color has length 3."
-- | Boolean inversion sets 0 to 1 and 1 to 0
interpQuilt env (Una Not e) = \x y -> case interpQuilt env e x y of
    [0,_,_] -> [1,1,1]
    [1,_,_] -> [0,0,0]
    _       -> error
        "Unreachable error. Typechecking should ensure Color has length 3."
-- | Binary operators are interpreted using interpBOp. Types do not matter now
interpQuilt env (Bin op e1 e2)      = \x y -> interpBOp op
    (interpQuilt env e1 x y)
    (interpQuilt env e2 x y)
-- | For quilts, we scale the values of x and y so that each quadrant displays a full quilt
interpQuilt env (Quilt e1 e2 e3 e4) = \x y -> case (x >= 0, y >= 0) of
    (False, True)  -> interpQuilt env e1 (x * 2 + 1) (y * 2 - 1)
    (True, True)   -> interpQuilt env e2 (x * 2 - 1) (y * 2 - 1)
    (False, False) -> interpQuilt env e3 (x * 2 + 1) (y * 2 + 1)
    (True, False)  -> interpQuilt env e4 (x * 2 - 1) (y * 2 + 1)
-- | Vars are retrieved from the env. Typchecking ensures the variable is defined
interpQuilt env (Var s) = case M.lookup s env of
    Just c -> c
    Nothing -> error "Unreachable error. Typechecking should ensure variable exists."
-- | e1 is interpreted and inserted into the env, then e2 is interpreted using the new env
interpQuilt env (Let s e1 e2) = interpQuilt (M.insert s (interpQuilt env e1) env) e2

-- | `interpClrLit` maps `CLit` values to `Color` values
interpClrLit :: CLit -> Color
interpClrLit White = [1.0, 1.0, 1.0]
interpClrLit Black = [0.0, 0.0, 0.0]
interpClrLit Red = [1.0, 0.0, 0.0]
interpClrLit Orange = [1.0, 0.5, 0.0]
interpClrLit Yellow = [1.0, 1.0, 0.0]
interpClrLit Green = [0.0, 1.0, 0.0]
interpClrLit Blue = [0.0, 0.0, 1.0]
interpClrLit Indigo = [0.29, 0, 0.51]
interpClrLit Violet = [0.56, 0.0, 1.0]
interpClrLit Grey = [0.5, 0.5, 0.5]

{- | `interpBOp` takes advantage of the fact that all types are interpreted as `Color`
values to have a single function to interpret all three types. Depending on the type of
operator, it applys a different function.
-}
interpBOp :: BOp -> Color -> Color -> Color
interpBOp (ABOp a) [r1,g1,b1] [r2,g2,b2] =
    [interpArith a r1 r2, interpArith a g1 g2, interpArith a b1 b2]
interpBOp (BBOp a) c1 c2                 =
    fromBoolean (interpBoolOp a (toBoolean c1) (toBoolean c2))
interpBOp (CBOp a) [r1,_,_] [r2,_,_]     =
    fromBoolean (interpCompare a r1 r2)
interpBOp _ _ _                          =
    error "Unreachable error. All colors should be match [_,_,_]"

{- | `interpArith`, `interpCompare`, and `interpBoolOp` map quilt operators to haskell
operators
-}
interpArith :: Arith -> (Double -> Double -> Double)
interpArith Add = (+)
interpArith Sub = (-)
interpArith Mul = (*)
interpArith Div = (/)

interpCompare :: Compare -> (Double -> Double -> Bool)
interpCompare Eq      = (==)
interpCompare NotEq   = (/=)
interpCompare Great   = (>)
interpCompare Less    = (<)
interpCompare GreatEq = (>=)
interpCompare LessEq  = (<=)

interpBoolOp :: BoolOp -> (Bool -> Bool -> Bool)
interpBoolOp And = (&&)
interpBoolOp Or  = (||)

-------------------------------- Pretty Printing -------------------------------

-- | `prettyQuilt` pretty prints `QExp` expressions
prettyQuilt :: QExp -> String
prettyQuilt (Num n)             = show n
prettyQuilt (BoolLit b)         = show b
prettyQuilt (ClrLit cl)         = prettyColorLit cl
prettyQuilt (CoordLit X)        = "x"
prettyQuilt (CoordLit Y)        = "y"
prettyQuilt (ClrExp e1 e2 e3)   =
    "[ "++prettyQuilt e1++", "++prettyQuilt e2++", "++prettyQuilt e3++" ]"
prettyQuilt (If e1 e2 e3)       =
    "if "++prettyQuilt e1++" then "++prettyQuilt e2++" else "++prettyQuilt e3
prettyQuilt (Una Neg e)         = "-"++prettyQuilt e
prettyQuilt (Una Not e)         = "!"++prettyQuilt e
prettyQuilt (Bin op e1 e2)      = prettyQuilt e1++prettyBin op++prettyQuilt e2
prettyQuilt (Quilt e1 e2 e3 e4) = 
    "quilt "++prettyQuilt e1++prettyQuilt e2++prettyQuilt e3++prettyQuilt e4
prettyQuilt (Var s)             = s
prettyQuilt (Let s e1 e2)       = "let "++s++" = "++prettyQuilt e1++" in "++prettyQuilt e2

-- | `prettyColorLit` maps `QExp`s to color strings
prettyColorLit :: CLit -> String
prettyColorLit White = "white"
prettyColorLit Black = "black"
prettyColorLit Red = "red"
prettyColorLit Orange = "orange"
prettyColorLit Yellow = "yellow"
prettyColorLit Green = "green"
prettyColorLit Blue = "blue"
prettyColorLit Indigo = "indigo"
prettyColorLit Violet = "violet"
prettyColorLit Grey = "grey"

-- | `prettyBin` maps operators to strings
prettyBin :: BOp -> String
prettyBin (ABOp Add)     = " + "
prettyBin (ABOp Sub)     = " - "
prettyBin (ABOp Mul)     = " * "
prettyBin (ABOp Div)     = " / "
prettyBin (BBOp And)     = " && "
prettyBin (BBOp Or)      = " || "
prettyBin (CBOp Eq)      = " == "
prettyBin (CBOp NotEq)   = " != "
prettyBin (CBOp Great)   = " > "
prettyBin (CBOp Less)    = " < "
prettyBin (CBOp GreatEq) = " >= "
prettyBin (CBOp LessEq)  = " <= "

-- | `prettyTypeError` pretty prints errors produced during typechecking
prettyTypeError :: TypeError -> String
prettyTypeError (Mismatch e1 e2 t1 t2) = 
    "Expression `"++prettyQuilt e2++"` in expression `"++prettyQuilt e1++
    "` has incorrect type\n  Expected type: "++show t1++"\n  Actual type: "
    ++show t2++"."
prettyTypeError (Incompat e1 t1 t2) =
    "Incompatible types in expression `"++prettyQuilt e1++"`\n  Type "
    ++show t1++" does not match type "++show t2
prettyTypeError (Undef s) = "Variable "++s++" is undefined"

-------------------------------------- Eval ------------------------------------

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

-- | Right now, this function ignores the input and simply produces a
--   blue image.  Obviously, you should make this function more
--   interesting!
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parse quilt s of
    Right q -> case infer M.empty q of
        Right _    -> Right (interpQuilt M.empty q)
        Left tyErr -> Left (prettyTypeError tyErr)
    Left e  -> Left (show e)
