> {-# LANGUAGE GADTs #-}

> import qualified Data.Map as M
> import           Parsing  hiding ((<$), (<$>), (<*>), (*>), (<*))
> import           Prelude
import qualified Control.Applicative as not
  
Module 10: Static types
=======================

* Write your team names here: Ted, Sarah, Thomas

In this module, we will continue our exploration of *type systems*.

Once again, we will build on the Arith language from previous
weeks.

* Choose any driver to start. Sarah

* Begin by copying **only your AST types and parser** (and desugaring,
  if applicable) from module 09 below.  Do **not** copy your
  interpreter, since we will not be extending it, but instead redoing
  it in a rather different style.

> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   Var :: String -> Arith
>   Let :: String -> Arith -> Arith -> Arith
>   BoolLit :: Bool -> Arith
>   If :: Arith -> Arith -> Arith -> Arith
>   deriving (Show)

> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Div   :: Op
>   Less  :: Op
>   Equiv :: Op
>   deriving (Show, Eq)
>
> data Value where
>   IntResult  :: Integer -> Value
>   BoolResult :: Bool -> Value
>   deriving Show
>
> data Type where
>   TyInt :: Type
>   TyBool :: Type
>   deriving (Show, Eq)
>
> data Error where
>   Undef :: String -> Error
>   TyError :: Arith -> Type -> Type -> Error
>   deriving Show
>
> type Ctx = M.Map String Type
>
>
> infer :: Ctx -> Arith -> Either Error Type
> infer ctx (Bin op a1 a2) = case bopType op of
>   (ty1, ty2, ty3) -> check ctx a1 ty1 *> check ctx a2 ty2 *> Right TyInt
> infer ctx (Lit a1) = Right TyInt
> infer ctx (BoolLit a1) = Right TyBool
> infer ctx (Var a1) = case M.lookup a1 ctx of
>  Just t -> Right t
>  Nothing -> Left (Undef a1)
> infer ctx (If test a1 a2) = 
>  check ctx test TyBool *>
>  infer ctx a1 >>= \t1 ->
>  check ctx a2 t1 *> Right t1
> infer ctx (Let st a1 a2) =
>  infer ctx a1 >>= \t1 ->
>  infer (M.insert st t1 ctx) a2
>
> bopType :: Op -> (Type, Type, Type)
> bopType Equiv = (TyInt, TyInt, TyBool)
> bopType Less = (TyInt, TyInt, TyBool)
> bopType _ = (TyInt, TyInt, TyInt)
>
> check :: Ctx -> Arith -> Type -> Either Error ()
> check ctx a1 ty = 
>  infer ctx a1 >>= \ty1 -> 
>  if ty == ty1 
>  then Right () else Left (TyError a1 ty ty1)
>
> inferArith :: Arith -> Either Error Type
> inferArith = infer M.empty 

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

Static type checking
====================

We are going to add a new **type checking** phase in between parsing
and interpreting. (**Note**, if you did the optional extension in the
previous module to add extra operators like `&&`, `>=`, and so on,
read this
footnote.^[If you desugar the new operators directly inside the parser, you will necessarily be running the type checker over the simpler, desugared syntax, which means that any type errors will refer to desugared programs. This is confusing for the programmer, because they may get errors about programs they did not write.  For example, if the programmer writes `True && 3`, it will first desugar to `if True then 3 else False`, and they will then get a type error referring to this `if`-expression even though they never wrote any `if`.  This is one good reason to have desugaring as a separate phase from parsing: first, the more complex language (the "surface language") is parsed into an AST which can represent the complete syntax of the surface language; this is then typechecked, so any type errors refer to the actual program the programmer wrote; a desugaring function then translates the successfully typechecked AST into the simpler language (the "core language"); finally, the interpreter (or compiler, optimizer, *etc*.) can work directly with the simplified core language.  Thankfully, in this case, writing a typechecker for the language extended with a bunch of extra operators is only slightly more work than writing a typechecker for the simpler language. The hard parts are things like `if` and `let`; adding more operators is relatively trivial.])
The type checker should adhere to the **Central Dogma of
Typechecking**:

**If type checking succeeds, then the program is a Good Program**.

Or, more pithily,

**Well-typed programs don't go wrong**.^[This phrase is due to Robin Milner, though in the original context he uses it with a precise technical meaning, not as a general guiding principle for type systems.  See Milner, Robin. "[A theory of type polymorphism in programming](http://www.research.ed.ac.uk/portal/files/15143545/1_s2.0_0022000078900144_main.pdf)." Journal of computer and system sciences 17.3 (1978): 348-375.]

Remember that in general, the designers of a language get to decide
  what "Good" means; in our case, we just want to make sure that our
  programs never do any nonsensical operations like add a boolean and
  an int.)

* First, we will need a Haskell data type to represent types in Arith.
  Create a new data type called `Type` with two constructors to
  represent the two types in Arith, namely, integers and booleans.
  These constructors do not need any fields since they do not store
  any information --- they simply represent the types `Bool` and
  `Int`.  I suggest using names such as `TyBool` and `TyInt` (don't
  call them `Bool` and `Int` since that would be super confusing).  Be
  sure to add the line `deriving (Show, Eq)` at the end of your
  definition so that `Type` values can be printed and compared for
  equality.

* We will also need a data type to represent errors that can be
  generated during type checking.  For now, it should have two
  constructors:

    - One to represent the occurrence of an undefined (or "unbound")
      variable.  It should store a `String` representing the name of
      the undefined variable.

    - One to represent a type error.  It should contain three fields:
      an `Arith` (representing the expression which failed to
      typecheck), and two `Type`s (representing the type the
      expression was expected to have, along with its actual type).

* Remember that when writing an interpreter we need a way to deal with
  variables: when we get to a variable, how should it be interpreted?
  We solved this problem by adding an *environment* which was a
  mapping from variables to values.  We have a similar problem when
  type checking: when we get to a variable, what type should it have?
  The solution is also similar: we need a *type context* which is a
  mapping from variables to *types*. Define `type Ctx = M.Map String
  Type`.

* Now write the type checker itself!  You should implement two
  main functions, `infer` and `check`, which are described below.  I
  have also included lots of other notes and hints to help you with
  the implementation; I suggest at least skimming through all the
  notes before starting to implement `infer` and `check`.

    - `infer :: Ctx -> Arith -> Either TypeError Type` takes a typing
      context and an `Arith` expression, and *infers*, i.e. figures
      out, the type of the expression (or generates an error if the
      expression has no type).

    - `check :: Ctx -> Arith -> Type -> Either TypeError ()` takes a
      typing context, an `Arith` expression, and a type, and *checks*
      to make sure that the expression has the given type.  It throws
      an error if either the expression does not have a type, or if it
      has a type which is different than the given one.  Note that it
      returns a value of type `()` (unit), which carries no useful
      information: we only care whether there are any errors or not.

    Some notes:

    - `check` should be very easy to implement: just `infer` the type
      of the given `Arith` expression, and then test whether its
      inferred type is equal to the given type.

    - `infer` can make use of `check` in situations where you know some
      `Arith` expression needs to have a certain type.  For example, if
      you encounter an expression `a + b`, you should `check` that both
      `a` and `b` have type Integer.

    - Yes, you read that right: `check` and `infer` should be *mutually
      recursive*, that is, each calls the other.

    - Remember that for parsers, `p1 *> p2` runs parser `p1` and then
        throws away its output before running `p2`.  In this context, if
        `t1` and `t2` are expressions that could throw a type checking
        error, `t1 *> t2` will run `t1` and make sure it does not generate
        an error before running `t2`.  This is especially useful for
        calling `check` (which does not return a useful result anyway).
        For example, you might have something like this to infer the type
        of `a + b`:

        ```
        infer ctx (Bin Add a b) =
          check ctx a TyInt *>       -- make sure a has type Int
          check ctx b TyInt *>       -- make sure b has type Int
          Right TyInt                -- If no errors were generated,
                                     --   finally return Int as the type
                                     --   of the whole expression (a + b)
        ```

    - `infer` will also need to make use of `(>>=)`.  For example,
      when inferring the type of an `if` expression, you will need to
      infer the types of its `then`- and `else`-parts:

        ```
        infer ctx (If test t1 t2) =
          ... other stuff ...
          infer ctx t1 >>= \ty1 ->
          ... some more stuff here that can use ty1 ...
        ```

    - The types of all the binary operators should be evident.
      *Hint*: You might find it useful to make a function `bopType ::
      Op -> (Type, Type, Type)` which returns the expected input types
      and the output type of a binary operator.

    - The most interesting cases are how to typecheck `if`-expressions
      and `let`-expressions.  Rather than tell you how they are done,
      I will let you think about them---in principle, you should be
      able to derive the correct implementation by thinking about how
      these expressions are interpreted, and by looking at the type
      rules that we wrote in our previous class. But feel free to ask
      me questions.

* Finally, make a function `inferArith :: Arith -> Either TypeError
  Type` which simply runs `infer` starting with an empty typing
  context.

![](../images/stop.gif)

Interpreter
===========

* **ROTATE ROLES** and write the name of the new driver here: Thomas 

* After typechecking has succeeded, can we still get runtime errors?
  If so, which runtime errors can still happen?  Which are no longer
  possible?

-- Type errors and undefined errors can no longer happen! Yay!
-- We can still get runtime errors such as dividing by zero. Not very yay.

Given a static type checker, we can do something very interesting with
the interpreter.  The whole idea of static type checking is that if a
program successfully type checks, there will never be any type errors
at runtime, so checking that types match up while interpreting the
program would be a complete waste of time.

One might think, however, that this runtime type checking is
unavoidable: our programs can evaluate either to booleans or integers,
so we need the `Value` type to represent the two possibilities.  Then
we have to pattern-match on `Value`s all the time to make sure we have
the right kind of value (even though we know that one of the cases
cannot happen).

However, we can use a trick: we can make the interpreter just return
`Integer` again, and *encode* boolean values as integers. For example,
encode `False` as 0, and `True` as 1.  When the interpreter is
running, there is no way to tell whether a particular `Integer` value
is supposed to represent an actual integer or a boolean---we say that
the type information has been *erased*.  This means that *in theory*,
the interpreter could perform nonsensical operations such as adding
`3 + True` (resulting in `4`).  However, the interpreter will only
ever run programs which successfully typecheck; if the typechecker is
implemented correctly, then cases like this can never actually happen!
Such errors would have been caught as type errors. (Ideally, this is
something we could *formally prove* about our system.)  This is one
reason static type checking can lead to better runtime performance: we
don't have to carry around type information and constantly check types
of values at runtime, which can remove a lot of runtime overhead (as
well as create better opportunities for optimization).

* Begin by copying your interpreter from **module 8** and pasting it
  here (*not* your dynamic typechecking interpreter from the previous
  module).

> type Env = M.Map String Integer

> interpArith3 :: Arith -> Env -> Integer
> interpArith3 (Lit i) env = i 
> interpArith3 (Bin Plus e1 e2) env = (+) (interpArith3 e1 env) (interpArith3 e2 env)
> interpArith3 (Bin Times e1 e2) env = (*) (interpArith3 e1 env) (interpArith3 e2 env)
> interpArith3 (Bin Minus e1 e2) env = (-) (interpArith3 e1 env) (interpArith3 e2 env)
> interpArith3 (Bin Div e1 e2) env = case interpArith3 e2 env of
>   0 -> error "Bug! 0! I don't like 0!"
>   n -> div (interpArith3 e1 env) n
> interpArith3 (Var var) env = case M.lookup var env of
>   Just m  -> m  
>   Nothing -> error ("Bug! Undefined variable!" ++ var ++ "Go define it and then come back!")
> interpArith3 (Let var e1 e2) env = interpArith3 e2 (M.insert var (interpArith3 e1 env) env)
> interpArith3 (Bin Less e1 e2) env = if interpArith3 e1 env < interpArith3 e2 env then 1 else 0
> interpArith3 (Bin Equiv e1 e2) env = if interpArith3 e1 env == interpArith3 e2 env then 1 else 0
> interpArith3 (BoolLit b) env = if b then 1 else 0
> interpArith3 (If yaf yif yuf) env = if interpArith3 yaf env == 1 then interpArith3 yif env 
>   else interpArith3 yuf env


* Remove any `InterpError`s which cannot happen anymore.  Replace them
  in the interpreter with calls to `error`, with a message like `"Bug!
  This case should be impossible!  Fizzwoz cannot happen in the
  interpreter, since bar baz."` (of course, you should replace the
  nonsense with something appropriate).

* Now extend `interpArith` appropriately.  Remember that in the case
  that you need a boolean value, you should just *assume* that you have
  an `Integer` which is either `0` or `1` representing `False` or
  `True`.

* Finally, update the `eval` function.  It should deal appropriately with
  parse errors, type errors, runtime errors, and with showing the
  output of the interpreter.  For example, here is what my solution
  looks like (note that depending on whether you did the optional
  extension, yours may not be able to handle things like `||`, `<=`,
  and so on):

> eval :: String -> IO ()
> eval s = case parse arith s of
>   Left err  -> print err
>   Right a   -> case infer M.empty a of
>     Right TyInt  -> print (interpArith3 a M.empty)
>     Right TyBool -> print (interpArith3 a M.empty == 1)
>     Left err     -> print (showErrorMessage err)

> showErrorMessage :: Error -> String
> showErrorMessage (Undef var) = "Error! Undefined variable! Go define it and then come back! Scram!"
> showErrorMessage (TyError a ty1 ty2) = "Error! Type Error! Go double check your types and then come back! Scram!"

    ```
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in x"
    7
    λ> eval "let x = 3 in ! (x < 8)"
    False
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in z"
    Undefined variable z
    λ> eval "let y = False in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else 9*10) in x"
    Type mismatch: Var "y" was used in a context that expected TInt,
      but it actually has type TBool.
    λ> eval "let y = 2 in let x = (if (3 > 5) || (y*3 <= 10) then 5+y else False && True) in x"
    The branches of the if expression If (Bin FOr (Bin FGt (ILit 3) (ILit 5)) (Bin FLeq (Bin FTimes (Var "y") (ILit 3)) (ILit 10)))
      (Bin FPlus (ILit 5) (Var "y"))
      (Bin FAnd (BLit False) (BLit True))
      should have the same type, but they have types TInt and TBool respectively.
    ```

    A few notes:

    - To get really nice type error messages, one would have to
      include a pretty-printer.  As you can see, I have not done so,
      but you are welcome to add one if you wish.
    - As you can see, I included a special kind of type error for
      `if`-expressions; you can do this if you wish but it is
      optional.
    - Note that one cannot simply `show` the result of the
      interpreter, since this would sometimes *e.g.* show `0` when it
      should be `False`.  To correctly show the output of the
      interpreter, one needs to take the inferred type of the
      expression into account.

* Try evaluating `if False then 3 else True`.  What happens?  How is
  this different than the previous module?

Feedback
--------

* How long would you estimate that you spent working on this module?
~1 hour

* Were any parts particularly confusing or difficult?
eval was a little confusing at first, but then we figured it out. 

* Were any parts particularly fun or interesting?
Writing the error messages was really fun! I like to think that it's an old man telling young whippersnappers to get off his lawn 
and to not come back until they've defined their variables and checked their types. That's why he says scram!

* Record here any other questions, comments, or suggestions for
  improvement.
Nothing else!
