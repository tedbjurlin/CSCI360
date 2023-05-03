Project 2: Calculator
=====================

For this project, you will implement the guts of a (fancy) calculator.
I have provided you with a simple read-eval-print interface (in
[CalcREPL.hs](CalcREPL.hs)) that lets the user type in expressions to
be evaluated.  You will ultimately provide a function of type `String
-> String` which accepts the user's input and produces a response.  Of
course, your `String -> String` function should be decomposed into
multiple phases, just like all of the language implementations we have
been considering (such as parsing, pretty-printing, interpreting, and
so on).  This project intentionally gives you much less guidance than
the first project in terms of what specific data types and functions
you should write, and how to decompose your solution.  However, you
can of course use the modules we have done as templates to help guide
you.

Getting started
---------------

- Download the [provided zip file](calc.zip), which should contain several
  files including [`CalcREPL.hs`](CalcREPL.hs),
  [`Calc.lhs`](Calc.lhs), [`Parsing2.hs`](Parsing2.hs), and a
  few configuration files such as `calc.cabal` and `stack.yaml`.

- Extract the contents of the zip file.

- If you do not have Haskell working on your computer you can use
  `replit.com` to complete this project. Simply upload all the
  provided files to a new `replit.com` project.

- While working on your calculator, to load it into `ghci` (e.g. in
   order to try out a function), you can use the `stack repl` command.

- To compile and run your calculator, you can use the command `stack run`
  (this should be typed at a terminal/shell prompt, not at a ghci
  prompt).

    - You should get a calculator prompt where you can enter expressions (though
      it will not do anything yet).

    - Simply exit the calculator and rerun `stack run` every time
      you want to test changes you have made to `Calc.lhs`.

Level 1
-------

Your calculator must support the following features:

- Floating-point numbers (represented as `Double` values)
- Standard arithmetic operations `+`, `-`, `*`, `/`, and `^`
  as well as prefix negation
- Display appropriate error messages to the user instead of crashing
- Display appropriate startup and `:help` messages which
  explain/illustrate the features of the calculator.

For example, a sample interaction with your calculator might look like
this:

    > 2+3
    5
    > (((3*5)   -   9)  + -8.3)
    -2.3000000000000007
    > 2 ^ 2 ^ 2 ^ 2
    65536.0
    > (3+3)*3
    18.0
    > 3+
    (line 1, column 3):
    unexpected end of input
    expecting end of "+", number, or "("

Your calculator must never crash with a runtime error or
pattern-match failure.

Get started by editing the starter code below and adding to it as
appropriate!

General notes and hints
-----------------------

+ You can use the `reserved` token parser to parse things like
  function names, names of constants or units, *etc.*
+ You can use the `naturalOrFloat` token parser to parse literal
  values that can either be an integer or a floating-point value. Note
  it does not handle negatives; that should be taken care of
  automatically by your prefix negation operator.
+ You can use `fromIntegral` to convert from `Integer` to `Double`.
+ You should use the `parse` function to run your parser.  If it
  returns an error wrapped in a `Left` constructor, you can simply
  call `show` on the resulting error to turn it into a `String`
  appropriate for displaying to the calculator user.
+ The `parseSome` function can be used as before for experimenting
  with parsers in GHCi.
+ Exponentiation for `Double` values in Haskell is done with the
  `(**)` operator.  (The `(^)` operator is only for integers.)

Starter code
------------

> {-# LANGUAGE GADTs #-}
>
> {-# OPTIONS_GHC -Wall #-}
>
> module Calc where
>
> import           Parsing2
> import           Data.Complex

Edit this description and replace it with your own!  It gets printed
when the calculator interface first starts up.

> description :: String
> description = unlines
>   [ ""
>   , "Are you ready to calculate? You've come to the"
>   , "right place!"
>   , ""
>   , "This amazing calculator supports tons of cool features,"
>   , "such as trigonometric functions, complex numbers and more!"
>   , "Type :help for more detailed information."
>   , ""
>   , "Type an expression, :help, or :quit to begin!"
>   ]

Edit this help message and replace it with your own! It gets printed
when the user types `:help`.  Adding some well-chosen examples could
be a good way to concisely show off the different features of your
calculator.

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values,"
>   , "negation, or standard arithmetic operators + - * / ^ ."
>   , ""
>   , "The functuions sin, cos, tan, cot, sec, csc, abs,"
>   , "log, and sqrt are also supported and should be called"
>   , "with parenthesis. For example:"
>   , ""
>   , "> sin(2)"
>   , ""
>   , "The constants e and pi are supported. Pi can be used"
>   , "with either the word pi or the symbol π."
>   , ""
>   , "Complex numbers are suported with either of the forms"
>   , "n + mi or mi, where the symbol i is used to differentiate"
>   , "complex numbers from reals. For example:"
>   , ""
>   , "> 2 + 3i"
>   , "> 4i"
>   , ""
>   , "Type :help to display this message, or :quit to quit."
>   ]

This is the main function that is called by `CalcREPL` to evaluate
user input.

> calc :: String -> String
> calc input = case parse (whiteSpace *> parseCalc <* eof) input of
>     Right c -> case interpCalc c of
>         Right n -> prettyCalc c ++ "\n  = " ++ prettyOut n
>         Left e  -> showInterpError e
>     Left  e -> show e
>
> data Calc where
>     Num  :: Complex Double -> Calc
>       -- The main constructor for numbers is of the type complex double to
>       -- allow for complex numbers. It was easier to treat all numbers as
>       -- complexes rather than have a seperate constructor for real numbers.
>       -- This way I can store reals as a complex number with a complex 
>       -- component of 0.
>     Con  :: Constant -> Calc
>     CCon :: Constant -> Calc
>       -- The `CCon` constructor was created to allow constants to be stored
>       -- printed as complex numbers. For example: πi or ei
>     Fnc  :: Function -> Calc -> Calc
>     Una  :: Op -> Calc -> Calc
>       -- The unary operation constructor was originally going to also handle
>       -- unary functions such a sin, but it became easier to interpret if 
>       -- I had a seperate constructor for that, so `Una` currently only
>       -- handles unary negation.
>     Bin  :: Op -> Calc -> Calc -> Calc
>     deriving Show
>
> data Function where
>       -- The `Function` adt allows for the quick and easy extension of the 
>       -- calculator with additional functions.
>     Sin  :: Function
>     Cos  :: Function
>     Tan  :: Function
>     Cot  :: Function
>     Sec  :: Function
>     Csc  :: Function
>     Abs  :: Function
>     Log  :: Function
>     Sqrt :: Function
>     deriving Show
>
> data InterpError where
>     DivByZero         :: InterpError
>     -- Indicates divide by zero errors.
>     IncorrectNumArgs  :: InterpError
>     -- Indicates the wrong number of arguments passed to an operator.
>     deriving Show
>
> data Op where
>     Add :: Op
>     Sub :: Op
>     Mul :: Op
>     Div :: Op
>     Exp :: Op
>     Neg :: Op
>     deriving (Show, Eq)
>
> data Constant where
>     E  :: Constant
>     Pi :: Constant
>     deriving Show
>
> type Precedence = Int
>
> data Associativity where
>     L :: Associativity
>     R :: Associativity
>     deriving (Show, Eq)
>
> prec :: Op -> Precedence
> prec Exp = 5
> prec Neg = 4
> prec Mul = 3
> prec Div = 3
> prec _   = 2
>
> assoc :: Op -> Associativity
> assoc Exp = R
> assoc _   = L
>
> lexer :: TokenParser u
> lexer = makeTokenParser $ emptyDef
>     { reservedNames = ["pi", "e", "π", "sin", "cos", "sqrt", "abs", "log", "tan"
>                       , "cot", "sec", "csc"] }
>     -- All of the functions and constants need to be added as reserved names.
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reserved :: String -> Parser ()
> reserved = getReservedOp lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> naturalOrFloat :: Parser (Either Integer Double)
> naturalOrFloat = getNaturalOrFloat lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> parseCalc :: Parser Calc
> parseCalc = buildExpressionParser table parseCalcAtom
>   where
>     table = [ [ Infix (Bin Exp <$ reservedOp "^") AssocRight ]
>             , [ Prefix (Una Neg <$ reservedOp "-") ]
>             , [ Infix (Bin Mul <$ reservedOp "*") AssocLeft 
>               , Infix (Bin Div <$ reservedOp "/") AssocLeft ]
>             , [ Infix (Bin Add <$ reservedOp "+") AssocLeft
>               , Infix (Bin Sub <$ reservedOp "-") AssocLeft
>               ]
>             ]
>
> parseFunction :: Parser Calc
> parseFunction 
>     =   (reserved "sin"  *> (Fnc Sin  <$> parens parseCalc))
>     <|> (reserved "cos"  *> (Fnc Cos  <$> parens parseCalc))
>     <|> (reserved "abs"  *> (Fnc Abs  <$> parens parseCalc))
>     <|> (reserved "log"  *> (Fnc Log  <$> parens parseCalc))
>     <|> (reserved "tan"  *> (Fnc Tan  <$> parens parseCalc))
>     <|> (reserved "cot"  *> (Fnc Cot  <$> parens parseCalc))
>     <|> (reserved "sec"  *> (Fnc Sec  <$> parens parseCalc))
>     <|> (reserved "csc"  *> (Fnc Csc  <$> parens parseCalc))
>     <|> (reserved "sqrt" *> (Fnc Sqrt <$> parens parseCalc))
>
> eitherToDouble :: Either Integer Double -> Double
>     -- Converts an `Either Integer Double` to a `Double`.
> eitherToDouble (Left n)  = fromIntegral n
> eitherToDouble (Right n) = n
>
> parseCalcAtom :: Parser Calc
> parseCalcAtom
>     -- Making use of the (:+) operator for converting a real to a complex.
>     = (Num <$> try (((0:+) . eitherToDouble <$> naturalOrFloat) <* reserved "i"))
>     <|> (Num <$> ((:+0) . eitherToDouble <$> naturalOrFloat))
>     <|> parens parseCalc
>     -- Try parsing for complex constants happens before the real constants. I almost
>     -- forgot to make these `try` parsers.
>     <|> (CCon Pi <$ try (reserved "pi" <* reserved "i"))
>     <|> (CCon Pi <$ try (reserved "π" <* reserved "i"))
>     <|> (CCon E <$ try (reserved "e" <* reserved "i")) 
>     <|> (Con Pi <$ reserved "pi")
>     <|> (Con Pi <$ reserved "π")
>     <|> (Con E <$ reserved "e")
>     <|> parseFunction
>
> interpCalc :: Calc -> Either InterpError (Complex Double)
> interpCalc (Num n)        = Right n
> interpCalc (Con c)        = Right (interpCon c :+ 0)
> interpCalc (CCon c)       = Right (0 :+ interpCon c)
>     -- The only difference between `Con` and `CCon` in the interpreter is whether
>     -- the constant gets interpreted into the real or complex component of the
>     -- number. This allows me to use the interpCon function for both.
> interpCalc (Una op a)     = interpUnaOp op `eitherFn` interpCalc a
> interpCalc (Bin op a1 a2) = case (op, interpCalc a2) of
>   (Div, Right 0) -> Left DivByZero
>   (_, n)         -> interpBinOp op `eitherFn` interpCalc a1 <*> n
> interpCalc (Fnc fnc a)    = interpFunc fnc <$> interpCalc a
>
> eitherFn :: Either e (a -> b) -> Either e a -> Either e b
>     -- I created this to do something similar to the `<<$>>` operator, but to 
>     -- work with an Either e (a -> b). This is to allow the `interpBinOp` and
>     -- `interpUnaOp` functions to throw `interpError`s.
> eitherFn (Left e) _           = Left e
> eitherFn _ (Left e)           = Left e
> eitherFn (Right fn) (Right a) = Right (fn a)
>
> interpFunc :: Function -> Complex Double -> Complex Double
> interpFunc Sin n  = sin n
> interpFunc Cos n  = cos n
> interpFunc Abs n  = abs n
> interpFunc Log n  = log n
> interpFunc Tan n  = tan n
> interpFunc Cot n  = 1 / tan n -- no native trig co functions in Haskell
> interpFunc Sec n  = 1 / cos n
> interpFunc Csc n  = 1 / sin n
> interpFunc Sqrt n = sqrt n
>
> interpCon :: Constant -> Double
>     -- Any new constants get added to the interpreter here.
> interpCon Pi = pi
> interpCon E  = 2.7182818284
>
> interpBinOp :: Op -> Either InterpError (Complex Double -> Complex Double -> Complex Double)
> interpBinOp Sub = Right (-)
> interpBinOp Add = Right (+)
> interpBinOp Div = Right (/)
> interpBinOp Mul = Right (*)
> interpBinOp Exp = Right (**)
>     -- `IncorrectNumArgs` should never actually reach to user. I have added it to appease
>     -- GHC -Wall because not all of the `Op` constructors appear in `interpBinOp` due to
>     -- them being unary operators. The parser should prevent a unary operator from being
>     -- passed as a binary. This could also be fixed by splitting the operators into 
>     -- seperate unary and binary adts, but this would require small changes all over
>     -- the program, which I would like to avoid.
> interpBinOp _   = Left IncorrectNumArgs
>
> interpUnaOp :: Op -> Either InterpError (Complex Double -> Complex Double)
>     -- Could be combined into `interpCalc`, but left seperate to allow for the
>     -- extensibility of the caluclator.
> interpUnaOp Neg = Right negate
>     -- See `interpBinOp`.
> interpUnaOp _   = Left IncorrectNumArgs
>
> prettyCon :: Constant -> String
> prettyCon E  = "e"
> prettyCon Pi = "π"
>
> prettyOp :: Op -> String
> prettyOp Neg = "-"
> prettyOp Exp = " ^ "
> prettyOp Mul = " * "
> prettyOp Div = " / "
> prettyOp Add = " + "
> prettyOp Sub = " - "
>
> prettyFunc :: Function -> String
> prettyFunc Sin  = "sin"
> prettyFunc Cos  = "cos"
> prettyFunc Abs  = "abs"
> prettyFunc Log  = "log"
> prettyFunc Sqrt = "sqrt"
> prettyFunc Tan  = "tan"
> prettyFunc Cot  = "cot"
> prettyFunc Sec  = "sec"
> prettyFunc Csc  = "csc"
>
> prettyPrec :: Precedence -> Associativity -> Calc -> String
>     -- Mostly this function just adds spaces and formating to the expression.
> prettyPrec _ _ (Num n)      = prettyOut n
> prettyPrec _ _ (Con c)      = prettyCon c
> prettyPrec _ _ (CCon c)     = prettyCon c ++ "i"
> prettyPrec p _ (Una op a1)
>     -- I kind of hate how dificult this is to read, but I don't really know a way
>     -- to improve it. It is mostly because all the functions have similar names
>     -- (starting with pretty).
>     | p > prec op = '(' : prettyOp op ++ prettyPrec (prec op)(assoc op) a1 ++ ")"
>     | otherwise   = prettyOp op ++ prettyPrec (prec op) (assoc op) a1
> prettyPrec p a (Bin op a1 a2)
>     | p > prec op || ((p == prec op) && (a /= assoc op))
>     = '(' : prettyPrec (prec op) (assoc op) a1 ++ prettyOp op ++ prettyPrec (prec op)(assoc op) a2 ++ ")"
>     | otherwise
>     = prettyPrec (prec op) (assoc op) a1 ++ prettyOp op ++ prettyPrec (prec op) (assoc op) a2
> prettyPrec _ _ (Fnc fnc a1) = prettyFunc fnc ++ "(" ++ prettyPrec 0 L a1 ++ ")"
>
> prettyOut :: Complex Double -> String
>   -- Only displays non-zero components.
> prettyOut (n:+0) = show n
> prettyOut (0:+m) = show m ++ "i"
> prettyOut (n:+m) = show n ++ " + " ++ show m ++ "i"
>
> prettyCalc :: Calc -> String
> prettyCalc = prettyPrec 0 L
>
> showInterpError :: InterpError -> String
> showInterpError DivByZero        = "Error: You cannot divide by zero."
> showInterpError IncorrectNumArgs = "Error: Wrong number of args for operator."


Level 2
-------

To complete this project to Level 2, in addition to the requirements
for Level 1:

- Re-display a nicely formatted version of the user's input as
  confirmation of each computation.  For example, a sample interaction
  with your calculator might now look like this:

    ```
    > 2+3
    2.0 + 3.0
      = 5.0
    > (((3*5)  -   9)  + -8.3)
    3.0 * 5.0 - 9.0 + -8.3
      = -2.3000000000000007
    > 2 ^ 2 ^ 2 ^ 2
    2.0 ^ 2.0 ^ 2.0 ^ 2.0
      = 65536.0
    > (3+3)*3
    (3.0 + 3.0) * 3.0
      = 18.0
    ```

- Ensure that your code uses [good Haskell style](https://kowainik.github.io/posts/2019-02-06-style-guide).

- Make sure your code is simplified as much as possible, for example,
  without redundant pattern-matching.

- Turn on `{-# OPTIONS_GHC -Wall #-}` and make sure your code generates no warnings.

- Write informative, grammatically correct comments explaining your
   code, its operation, and any choices you made along with the
   reasons for those choices.

Level 3
-------

To complete this project to Level 3, in addition to the requirements
for Level 2, you must complete *at least two* extensions.  You may
pick from the following list of suggested extensions (ordered roughly
from easier to harder), or propose your own.

1. Add support for the constants $\pi$ and $e$, along with at least
   five functions such as sine, cosine, tangent, log, floor, ceiling,
   round, square root, or absolute value.  For example, a sample
   interaction might look like this:

    ```
    > sin(pi/6)
    sin(π / 6.0)
      = 0.49999999999999994
    > cos(tan(log(abs(-2))))
    cos(tan(log(abs(-2.0))))
      = 0.6744026976311414
    > ((1 + sqrt(5))/2)^2 - 1
    ((1.0 + sqrt(5.0)) / 2.0) ^ 2.0 - 1.0
      = 1.618033988749895
    ```

2. Support for complex numbers.  For example, the user should be able
   to enter expressions like `3 + 2i`.  Note that real numbers should
   never be pretty-printed with an imaginary component, and purely
   imaginary numbers should not be pretty-printed with a real
   component.  For example,

    ```
    > 2
    2.0
      = 2.0
    > 3i
    3.0i
      = 3.0i
    > i + 2
    i + 2.0
      = 2.0 + i
    > 2 + 3i
    2.0 + 3.0i
      = 2.0 + 3.0i
    > (2 + 3i) * (4 + 6i)
    (2.0 + 3.0i) * (4.0 + 6.0i)
      = -10.0 + 24.0i
    > sqrt(2 + 3i)
    sqrt(2.0 + 3.0i)
      = 1.6741492280355401 + 0.8959774761298381i
    ```

    (The last example works only if you have also implemented the
    first extension.)

    You can import the `Complex.Double` module to work with complex
    numbers in Haskell.

    Note there is a slight wrinkle to deal with when parsing a literal
    imaginary value: if you see a number you do not yet know whether
    it will be followed by `i` or not.  The problem is that by
    default, if a parsec parser consumes some input before failing, it
    does *not* backtrack to try re-parsing the same input.  So, as an example,
    something like this:

    ```
    Imag <$> (integer <* reserved "i") <|> Real <$> integer
    ```

    does *not* work, since if there is an integer not followed by an
    `i`, the first parser will irreversibly consume the integer before
    failing to find an `i`; when the second parser is tried there will
    no longer be an integer for it to find.

    The solution is that any parser which you would like to backtrack
    can be wrapped in the `try` function.  So

    ```
    Imag <$> try (integer <* reserved "i") <|> Real <$> integer
    ```

    works as expected: if there is no `i` following an integer and the
    first parser fails, the input gets rewound to the beginning of the
    integer before trying the second parser.

3. Support for units of measurement.  Pick a domain (*e.g.* length,
   mass, time, ...) and allow the user to add units in that domain to their
   calculations.  For example  (yours does not have to work exactly
   like this):

    ```
    > 1
    1.0
      = 1.0
    > 1 inch
    1.0 in
      = 1.0 in
    > 1 inch + 3 inches
    1.0 in + 3.0 in
      = 4.0 in
    > 1 meter + 1 inch
    1.0 m + 1.0 in
      = 1.0254 m
    > (1 meter + 1 inch) as inches
    (1.0 m + 1.0 in) as in
      = 40.370078740157474 in
    > ((1.6 ft * 700 + 8.1 ft) / 2) as miles
    ((1.6 ft * 700.0 + 8.1 ft) / 2.0) as mi
      = 0.10678412422360248 mi
    > 5 feet * 2 meters
    5.0 ft * 2.0 m
      = Error: tried to multiply two values with units, namely 5.0 ft and 2.0 m
    > 5 km + 6
    5.0 km + 6.0
      = Error: tried to add values with and without units, namely 5.0 km and 6.0
    > (5 km) mi
    5.0 km mi
      = Error: tried to apply units mi to a value that already had units km
    > (5 km) as mi
    5.0 km as mi
      = 3.105590062111801 mi
    > 6 as cm
    6.0 as cm
      = Error: can't convert scalar 6.0 to cm
    ```

    Some hints:

    + It should be possible to add two values with units, with
      conversion as appropriate.  It should be an error to add a value
      with units to a value without units.
    + It should be possible to multiply a value with units by a value
      without units, or vice versa.  It should be an error to multiply
      two values with units.
    + It is an error to do exponentiation with anything other than
      unitless values.
    + You will need to change your interpreter quite a bit: it will
      need to keep track of which values have units attached and which
      do not.  It also now has the possibility of generating a runtime
      error.
    + In the example above, units can be introduced by adding a unit
      to a value as a suffix: this makes a unitless value into a value
      with a unit, or checks that a value with units has the indicated
      units.  Alternatively, a conversion can be indicated by writing
      "as <unit>"; this convets a value with units into the indicated
      units, and is an error for values without units.  See the above
      examples.  This is just a suggestion; you do not have to
      organize your calculator in exactly this way.

4. Support for simple algebraic expressions involving polynomials.
   For example:

    ```
    > (x+1)^2
    (x + 1)^2
      = x^2 + 2*x + 1
    > (x+1)*(y-3)
    (x + 1) * (y - 3)
      = x * y - 3 * x + y - 3
    > (x^2 + 3*x + 1) / (x + 1)
      Sorry, division of polynomials is not supported.
    ```

    If you want to be really fancy you could support polynomial
    division too:

    ```
    > (x^2 + 3*x + 1) / (x + 1)
    (x^2 + 3 * x + 1) / (x + 1)
      = x + 2 - 1 / (x + 1)
    ```

5. You should also feel free to propose your own extensions; just be
   sure to run them by me to make sure you choose something with an
   appropriate level of difficulty.
