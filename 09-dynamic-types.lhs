Module 09: Dynamic types
========================

* Write your team names here: Ted, Sarah, Thomas

In this module, we will start exploring *type systems*, a central
topic in programming language design.

We will continue building on the Arith language from previous weeks.

* As the initial driver, pick the person with the most vowels in their
  name.  Break ties by playing rock, paper, scissors.

* You may either build upon your solution to Module 8, or [you may use
   mine](../code/ArithVars.lhs).  In either case, paste it here:

> {-# LANGUAGE GADTs #-}
>
> import qualified Data.Map as M
> import           Parsing  hiding ((<$), (<$>), (<*>), (*>), (<*))
> import           Prelude
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   Var :: String -> Arith
>   Let :: String -> Arith -> Arith -> Arith
>   BoolLit :: Bool -> Arith
>   If :: Arith -> Arith -> Arith-> Arith
>   deriving (Show)
>
> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Div   :: Op
>   Less :: Op
>   Equiv :: Op
>   deriving (Show, Eq)
>
> data Value where
>   IntResult  :: Integer -> Value
>   BoolResult :: Bool -> Value
>   deriving Show
>
> -- Parser
>
> lexer :: TokenParser u
> lexer = makeTokenParser $ emptyDef
>   { reservedNames = ["let", "in", "False", "True", "if", "then", "else"] }
>
> parens :: Parser a -> Parser a
> parens     = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> reserved :: String -> Parser ()
> reserved   = getReserved lexer
>
> integer :: Parser Integer
> integer    = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> identifier :: Parser String
> identifier = getIdentifier lexer
>
> parseArithAtom :: Parser Arith
> parseArithAtom =
>       Lit <$> integer
>   <|> Var <$> identifier
>   <|> BoolLit <$> parseBool
>   <|> parseIf
>   <|> parseLet
>   <|> parens parseArith
>
> parseLet :: Parser Arith
> parseLet = Let
>   <$> (reserved   "let" *> identifier)
>   <*> (reservedOp "="   *> parseArith)
>   <*> (reserved   "in"  *> parseArith)
>
> parseIf :: Parser Arith
> parseIf = If
>   <$> (reserved "if" *> parseArith)
>   <*> (reserved "then" *> parseArith)
>   <*> (reserved "else" *> parseArith)
>
> parseBool :: Parser Bool
> parseBool = True <$ reserved "True"
>   <|> False <$ reserved "False"
>
> parseArith :: Parser Arith
> parseArith = buildExpressionParser table parseArithAtom
>   where
>     table = [ [ Infix (Bin Times <$ reservedOp "*") AssocLeft
>               , Infix (Bin Div   <$ reservedOp "/") AssocLeft
>               ]
>             , [ Infix (Bin Plus  <$ reservedOp "+") AssocLeft
>               , Infix (Bin Minus <$ reservedOp "-") AssocLeft
>               ]
>             , [ Infix (Bin Less <$ reservedOp "<") AssocNone
>               , Infix (Bin Equiv <$ reservedOp "==") AssocNone
>               ]
>             ]
>
> arith :: Parser Arith
> arith = whiteSpace *> parseArith <* eof
>
> -- Interpreter
>
> type Env = M.Map String Value
>
> data InterpError where
>   UnboundVar :: String -> InterpError
>   DivByZero  :: InterpError
>   TypeError  :: InterpError
>   deriving Show
>
> showInterpError :: InterpError -> String
> showInterpError (UnboundVar x) = "Unbound variable " ++ x
> showInterpError DivByZero      = "Division by zero"
> showInterpError TypeError      = "Incorrect type"
>
> interpArith :: Env -> Arith -> Either InterpError Value
> interpArith _ (Lit i)           = Right (IntResult i)
> interpArith _ (BoolLit b)       = Right (BoolResult b)
> interpArith env (Var s)         = case M.lookup s env of
>   Just v  -> Right v
>   Nothing -> Left (UnboundVar s)
> interpArith env (Let s a1 a2)   = 
>   interpArith env a1 >>= \x ->
>   interpArith (M.insert s x env) a2
> interpArith env (Bin Plus a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   interpInt env a2 >>= \v2 ->
>   Right (IntResult (v1 + v2))
> interpArith env (Bin Minus a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   interpInt env a2 >>= \v2 ->
>   Right (IntResult (v1-v2))
> interpArith env (Bin Times a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   interpInt env a2 >>= \v2 ->
>   Right (IntResult (v1*v2))
> interpArith env (Bin Div a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   case v1 of
>     0 -> Left DivByZero
>     _           -> interpInt env a2 >>= \v2
>       -> Right (IntResult (div v1 v2))
> interpArith env (Bin Less a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   interpInt env a2 >>= \v2 ->
>   Right (BoolResult (v1<v2))
> interpArith env (Bin Equiv a1 a2) =
>   interpInt env a1 >>= \v1 ->
>   interpInt env a2 >>= \v2 ->
>   Right (BoolResult (v1==v2))
> interpArith env (If a1 a2 a3) = case interpBool env a1 of
>   Right True  -> interpArith env a2
>   Right False -> interpArith env a3
>   Left e      -> Left e
> --intValueOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Either InterpError Value
> --intValueOp op (IntResult i1) (IntResult i2) = Right (IntResult (op i1 i2))
> --intValueOp _ _ _                            = Left TypeError
>
> --boolValueOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Either InterpError Value
> --boolValueOp op (IntResult i1) (IntResult i2) = Right (BoolResult (op i1 i2))
> --boolValueOp _ _ _                            = Left TypeError
>
> interpBool :: Env -> Arith -> Either InterpError Bool
> interpBool env a = case interpArith env a of
>   Right (BoolResult b) -> Right b
>   Right _              -> Left TypeError
>   Left e               -> Left e
> 
> interpInt :: Env -> Arith -> Either InterpError Integer
> interpInt env a = case interpArith env a of
>   Right (IntResult i) -> Right i
>   Right _             -> Left TypeError
>   Left e              -> Left e
> -- Evaluator
>
> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left pErr  -> print pErr
>   Right e    ->
>     case interpArith M.empty e of
>       Left iErr -> putStrLn (showInterpError iErr)
>       Right v   -> print v

Extending the syntax
--------------------

* Begin by commenting out the interpreter and the `eval` function, so
  we can focus on extending and testing the abstract syntax and parser
  before we worry about updating the interpreter.

* Now, extend the abstract syntax and parser:

    - Begin by adding `False`, `True`, `if`, `then`, and `else` to
      the set of `reservedNames` in the `lexer`.

    - Now add Boolean literals `False` and `True`.

    - Add comparison operators `<` and `==`.  They should have lower
      precedence than `+` and `-` (but the same precedence as each
      other), and no associativity (`AssocNone`).

    - Finally, add an `if-then-else` construct like Haskell's.  That is, Arith
      expressions now include the syntax
        ```
        'if' <arith> 'then' <arith> 'else' <arith>
        ```
        You should add this to `parseArithAtom`.

* After extending your AST definitions and your parser, you can test
  it by trying to parse things like

    ```
    parse arith "if (3 == y) < z/2 then (if z then 3 else False) else True"
    ```

* (**Optional**, skip this at first and come back to
  it later if you wish.)  As a fun extra challenge, try adding other standard
  Boolean and comparison operators to your language such as `||`
  (logical or), `!` (logical negation), `>` (greater than), `<=` (less
  than or equal to), `>=` (greater than or equal to), and `!=` (not
  equal to).  The comparison operators should all have the same
  precedence as `==` and `<`; negation (`!`) should have higher
  precedence than comparisons; logical and (`&&`) should have lower
  precedence than comparisons and be right-associative; and local or
  (`||`) should have lower precedence than `&&` and also be right-associative.

    Of course one could again extend the abstract syntax and the
    parser, and deal with all the extra operations throughout the
    interpreter, type checker, *etc.*.  However, there is a better
    way: the set of existing features (`False`, `True`, less than,
    equal to, and `if-then-else`) was carefully chosen so that it is
    possible to express equivalent encodings of any of these other
    operators in terms of the existing ones.  For example, `x > y` can
    be encoded by simply flipping the order of arguments and using `y
    < x`; as another example, `x || y` can be encoded as `if x then
    True else y`.

    - One can parse to an AST `FullArith` containing all these
      operators, and then write a function `desugar :: FullArith ->
      Arith` which translates the language with all operators into the
      simpler language with only `False`, `True`, `<`, `==`, and
      `if`.

    - Alternatively, one can build the desugaring directly into the
      parser.  For example, to parse the greater-than operator, one
      could write something like

        ```
        , Infix ((\x y -> Bin Lt y x) <$ reservedOp ">") AssocNone
        ```

        assuming that `Lt` represents the less-than operation.  Note
        that `(\x y -> Bin Lt y x)` represents an *anonymous function*
        which takes two `Arith` arguments `x` and `y` and returns the
        `Arith` value `Bin Lt y x`.

![](../images/stop.gif)

Extending the interpreter
-------------------------

* **ROTATE ROLES** and write the name of the new driver here: Ted

Of course, the interesting thing we have done is to introduce the
possibility of a third kind of error, namely, type errors.  We now
have both integers and booleans in our language, and it would be an
error, say, to try to add two boolean values; addition only applies to
integers.

With type errors we have an interesting choice: should type errors be
caught *at runtime*, or should they be caught in a separate *type
checking* phase before the program is run?

- Checking for type errors at runtime is known as a *dynamic type
  system*.  It is considerably easier to implement, and takes as lenient
  an approach as possible.  Some people also claim that programming in
  such a language makes them more "agile".  These people are sadly
  deluded.

- Checking for type errors at compile time is known as a *static type
  system*. It is more complicated to implement, and necessarily
  disallows some programs that would have run successfully in a
  dynamically typed framework, but can lead to better runtime
  performance and more opportunities for runtime optimization.  It
  also means certain kinds of programmer errors can be caught before
  the program is run.

Today, you will implement dynamic type checking for our language; in
future modules you will implement a static type system.

* `Integer` is no longer a sufficient result type for the interpreter,
  since programs can now evaluate to either an integer *or* a boolean.
  Create a new type called `Value` with two constructors that
  encapsulate the possible results of evaluation.

* Currently, the `InterpError` type has two constructors, representing
  the possibility of encountering an undefined variable at runtime, or
  dividing by zero.  Add another constructor representing the
  possibility of a type error.  For now, this constructor will contain
  no information.  Later, when we implement a type checker, we will
  add more fields so we can generate better error messages.

* Now, the moment you have been waiting for: uncomment your
  interpreter and change its result type from `Either InterpError
  Integer` to `Either InterpError Value`.  Of course, it will no
  longer type check.  You should fix it, and extend it to interpret
  the new syntactic constructs we added.  Some notes/hints:

    - I suggest trying to uncomment and fix only one part at a time,
      so you can get each part to work before moving on to
      the next.

    - As an example of what you will have to fix, note that a line
      such as
        ```
        interpArith e (Bin Plus e1 e2) = (+) <$> interpArith e e1 <*> interpArith e e2
        ```
        will no longer work, since `(+)` expects two `Integer`s, but
        the recursive calls to `interpArith` now return `Value`s
        instead of `Integer`s.  Essentially you will have to replace
        `(+)` with a function of type `Value -> Value -> Either
        InterpError Value` which checks to make sure both values are
        integers and then adds them, throwing a type error if either
        value is not an integer.  (However, literally replacing `(+)`
        with such a function in the code above will not quite work either;
        consider using the `(>>=)` operator.)

    - You may assume the following:

        - The `==` and `<` operators expect two integers and return a
          boolean.  (It is possible to overload them so they also work
          on booleans, but we will not bother with that for now.)
        - `if-then-else` expects a boolean test after the `if`, and
          does not care about the types of its two branches.
        - `if-then-else` should **not evaluate both branches**.  It
          should *first* evaluate the test, and then decide which
          branch to evaluate (either `then` or `else`) based on the
          result of the test.  For example,
            ```
            if True then x else 3
            ```
            should result in an error saying that `x` is undefined,
            but
            ```
            if False then x else 3
            ```
            should successfully evaluate to `3`.

    - Try to abstract out common patterns in order to avoid writing
      repeated code.  As one example, I suggest making a function like
      `interpBool :: Env -> Arith -> Either InterpError Bool` (and
      similarly `interpInteger`) which interprets an expression and
      throws a type error if it does not result in a boolean.  You
      may come up with other ways to abstract as well.

* Finally, once you get your interpreter working, uncomment the `eval`
  function and get it working as well.

* Be sure to try your interpreter on a number of examples.  For example:
    ```
    if (3 == y) < z/2 then (if z then 3 else False) else True
    let x = 10 in (if (x < 12) then (if 5 == 5 then x + 19 else 12) else 8
    let y = 2 in if 3 < 5 then False else y
    ```

Feedback
--------

* How long would you estimate that you spent working on this module?

* Were any parts particularly confusing or difficult?

* Were any parts particularly fun or interesting?

* Record here any other questions, comments, or suggestions for
  improvement.
