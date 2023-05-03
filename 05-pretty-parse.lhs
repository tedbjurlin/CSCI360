> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -Wall #-}
> import Data.Char (isSpace, isDigit)

Module 05: The Arith language: pretty-printing and parsing
==========================================================

* Write your team names here: Ted Bjurlin, Taylor Aishman, Dalton Casey

* You may again choose whoever you want to start as the driver.  Write
  your choice here: Ted Bjurlin

Arith syntax and semantics
--------------------------

The Arith language is represented by the following abstract syntax:

> data Arith1 where
>   Lit1 :: Integer -> Arith1
>   Add  :: Arith1 -> Arith1 -> Arith1
>   Sub  :: Arith1 -> Arith1 -> Arith1
>   Mul  :: Arith1 -> Arith1 -> Arith1
>   deriving (Show)
>
> arithExample :: Arith1
> arithExample = Add (Mul (Lit1 4) (Lit1 5)) (Lit1 2)
>
> arithExample2 :: Arith1
> arithExample2 = Mul (Lit1 4) (Add (Lit1 5) (Lit1 2))

(We are using the name `Arith1` to avoid a name clash, since later we
will use a more refined version called `Arith`.)  The semantics of an
Arith expression is an integer: `Lit1` values represent themselves,
`Add` represents addition, `Sub` subtraction, and `Mul`
multiplication.  For example, `arithExample` evaluates to $(4 \times
5) + 2 = 22$, and `arithExample2` evaluates to $4 \times (5 + 2) = 28$.

* Write an interpreter called `interpArith1` for `Arith1` expressions.

> interpArith1 :: Arith1 -> Integer
> interpArith1 (Lit1 n)    = n
> interpArith1 (Add a1 a2) = interpArith1 a1 + interpArith1 a2
> interpArith1 (Sub a1 a2) = interpArith1 a1 - interpArith1 a2
> interpArith1 (Mul a1 a2) = interpArith1 a1 * interpArith1 a2

As concrete syntax for Arith, we use standard mathematical notation
and standard conventions about operator precedence. For example,
`"4*5+2"` is concrete syntax for `arithExample`, since by convention
multiplication has higher precedence than addition.  If we want
concrete syntax to represent `arithExample2`, we have to use
parentheses: `"4*(5+2)"`.

* Write a pretty-printer `prettyArith1` which turns Arith abstract
  syntax into valid concrete syntax.  At this point, you should try to
  make your pretty-printer as simple as possible rather than try to
  produce the best output possible.  (*Hint*: something like
  `"((4*5)+2)"` is perfectly valid concrete syntax, even
  though it has unnecessary parentheses.)

> prettyArith1 :: Arith1 -> String
> prettyArith1 (Lit1 n)     = show n -- https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string
> prettyArith1 (Add a1 a2) = '(':prettyArith1 a1++['+']++prettyArith1 a2++[')']
> prettyArith1 (Sub a1 a2) = '(':prettyArith1 a1++['-']++prettyArith1 a2++[')']
> prettyArith1 (Mul a1 a2) = prettyArith1 a1++['*']++prettyArith1 a2

* How might you go about altering your pretty printer to omit needless
  parentheses?  Write down some ideas here.

Add cases to check for parenthesis on the very outside by using a second funtion
to start the pretty printer. This function does the same thing as the inner
function, but does not add parenthesis to the string. It then calls the inner 
function instead of iteself when it recurses. Multiplication never needs
parenthesis around itself. An operation only need parenthesis if its parent is
multiplication. If the type of our pretty printer is Arith1 -> Bool -> String,
where the Bool argument is true if the parent is multiplication. In addition or
subtraction, it would add parenthesis if the Bool is true, and multiplication
never needs parenthesis.

![](../images/stop.gif)

A better pretty-printer
-----------------------

* **ROTATE ROLES** and write the name of the new driver here:

> data Op where
>   Plus  :: Op
>   Minus :: Op
>   Times :: Op
>   Exp   :: Op
>   deriving (Show, Eq)
>
> data Arith where
>   Lit :: Integer -> Arith
>   Bin :: Op -> Arith -> Arith -> Arith
>   deriving (Show)
>
> data Associativity where
>   L :: Associativity
>   R :: Associativity
>   deriving (Show, Eq)
>
> type Precedence = Int
>
> -- 4 * (5 + 2)
> expr1 :: Arith
> expr1 = Bin Times (Lit 4) (Bin Plus (Lit 5) (Lit 2))
>
> -- 44 - 7 * (1 + 2) - 3
> expr2 :: Arith
> expr2 = Bin Minus (Lit 44) (Bin Minus (Bin Times (Lit 7) (Bin Plus (Lit 1) (Lit 2))) (Lit 3))
>
> -- 63 + 1 * 43 - 2 ^ (5 + 2 - 2 ^ 3)
> expr3 :: Arith
> expr3 = Bin Plus (Lit 63) (Bin Minus (Bin Times (Lit 1) (Lit 43)) (Bin Exp (Lit 2) (Bin Plus (Lit 5) (Bin Minus (Lit 2) (Bin Exp (Lit 2) (Lit 3))))))

* Compare `Arith` and `Arith1`.  How is `Arith` different/more general
  than `Arith1`?
  Arith is more scalable as Op is the operators IE addition, subraction, muliplication and can be added in later
  or expanded as needed

* Write an interpreter `interpArith` for `Arith` expressions. (*Hint*:
  try writing a separate function `interpOp :: Op -> (Int -> Int ->
  Int)` to factor out the behavior of different operators.)

> interOP :: Op -> (Integer -> Integer -> Integer)
> interOP Plus arg1 arg2  = arg1 + arg2
> interOP Minus arg1 arg2 = arg1 - arg2
> interOP Times arg1 arg2 = arg1 * arg2
> interOP Exp arg1 arg2   = arg1 ^ arg2


> interpArith :: Arith -> Integer
> interpArith (Lit n) = n
> interpArith (Bin sign arg1 arg2) = interOP sign (interpArith arg1) (interpArith arg2)

* Write functions `assoc :: Op -> Associativity` and `prec :: Op ->
  Precedence` to return the associativity and precedence of each
  operator.  Addition, multiplication, and subtraction are all
  left-associative by convention.  Addition and subtraction should
  have the same precedence level, with multiplication at a higher
  level (typically larger numbers represent higher precedence, that
  is, ``stickier glue'').

> assoc :: Op -> Associativity
> assoc Exp = R
> assoc _   = L

> prec :: Op -> Precedence
> prec Exp   = 4
> prec Times = 3
> prec _     = 2

* Now write a function `prettyPrec :: Precedence -> Associativity ->
  Arith -> String`.  The three arguments to `prettyPrec` represent:
    - The precedence level of the **parent** operator
    - Whether the current expression is a left or right child of its parent
    - The expression to be prett-printed
  Given these inputs, it
  should print out a properly parenthesized version of the expression,
  with parentheses surrounding the entire expression only if they are
  needed.  Remember that parentheses are needed when (and only when):

    - The precedence of the parent operator is higher than the
      precedence of the operator at the root of the current expression, OR
    - The precedence of the parent operator is equal to the precedence
      of the root operator, and the associativity of the root operator
      is the opposite of which side of its parent it is on.

> prettyPrec :: Precedence -> Associativity -> Arith -> String
> prettyPrec _ _ (Lit n)        = show n
> prettyPrec p a (Bin op a1 a2)
>   | p > prec op || ((p == prec op) && (a /= assoc op)) = '(':prettyPrec (prec op)
>     (assoc op) a1++printOp op++prettyPrec (prec op)(assoc op) a2++")"
>   | otherwise                                          = prettyPrec (prec op) 
>     (assoc op) a1++printOp op++prettyPrec (prec op) (assoc op) a2

> printOp :: Op -> String
> printOp Plus  = "+"
> printOp Minus = "-"
> printOp Times = "*"
> printOp Exp   = "^"

* Write a function `prettyArith :: Arith -> String` which works by
  calling `prettyPrec` with appropriate starting arguments.

> prettyArith :: Arith -> String
> prettyArith (Lit n)        = show n
> prettyArith (Bin op a1 a2) = prettyPrec (prec op) (assoc op) a1++printOp op++
>   prettyPrec (prec op) (assoc op) a2

* Now go back and add an exponentiation operator to the
  language. Exponentiation is right-associative and has higher
  precedence than multiplication. Be sure to update both the
  interpreter and pretty-printer appropriately.



Parsing
=======

In the rest of this module, we will explore some rudiments of parsing.

Tokenizing
----------

> data Token where
>   TLit   :: Integer -> Token
>   TPlus  :: Token
>   TMinus :: Token
>   TTimes :: Token
>   LParen :: Token
>   RParen :: Token
>   deriving (Show, Eq)

> tokenize :: String -> [Token]
> tokenize ""         = []
> tokenize (c : cs)
>     | isSpace c = tokenize cs
> tokenize ('(' : cs) = LParen : tokenize cs
> tokenize (')' : cs) = RParen : tokenize cs
> tokenize ('+' : cs) = TPlus : tokenize cs
> tokenize ('-' : cs) = TMinus : tokenize cs
> tokenize ('*' : cs) = TTimes : tokenize cs
> tokenize (d : cs)
>     | isDigit d = case span isDigit (d:cs) of
>         (digits, rest) -> TLit (read digits) : tokenize rest
> tokenize (c: _) = error("Invalid Character: " ++ [c])

* Write a function `tokenize :: String -> [Token]` which turns a
  string such as `" ( 2+ 45)"` into a list of tokens like
  `[LParen, TLit 2, TPlus, TLit 45, RParen]`.  Your function should
  ignore any whitespace (use the `isSpace` function).  *Hint*: you may
  also find the `span` function useful.  Try calling `span isDigit` to
  see what it does.

    Of course, `tokenize` might fail if the provided `String` is not a
    valid Arith expression.  Ideally, we would handle the errors in a
    principled way (*e.g.* by having `tokenize` return a `Maybe
    [Token]`, or something even more informative). For now, your
    function should simply crash when given invalid input.  We will
    consider error handling in more depth later in the semester.

Postfix
-------

* **ROTATE ROLES** and write the name of the new driver here:

Consider the following alternative *postfix* syntax for Arith:

```
<digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
<num>   ::= <digit>+
<op>    ::= '+' | '-' | '*'
<post>  ::= <num>
          | <post> ' ' <post> ' ' <op>
```

Notice that each operator appears *after* its operands (instead of
*between* them).

* Write down three examples that match the grammar `<post>`.

12 3 +

44 5 - 3 +

13 55 *

* Consider the following infix expressions.  Write each as an
  equivalent postfix expression.  Be careful about the order of arguments!

    + `3 + 4 * 5`

    2 4 5 * +

    + `3 * 4 + 5`

    3 4 * 5 +

    + `(2 + 3 - (4 - 5)) * 6`
    
    2 3 + 4 5 - - 6 *

* Get out a sheet of paper and draw each of the abstract syntax trees
  corresponding to the above expressions.  What is the relationship
  between their trees and their postfix forms?
  
  The postfix syntax is effectively a way of writing the abstract syntax tree, where
  the right side is the top of the tree and the tree can be processed from bottom
  to top be reading the expression from left to right.

* Note that `<post>` expressions do not contain any parentheses.
  Will this ever cause ambiguity or incorrect parsing?  If so, give an
  example; if not, explain why not.

  Because the postfix form reflects the tree structure of the abstract syntax trees, there isn't any
  ambiguity in parsing. As long as the operators are processed left to right, they should
  always be done in the proper order. 

Shunting
--------

* **ROTATE ROLES** and write the name of the new driver here:

As discussed in class, the Shunting Yard Algorithm reorders a list
of tokens from infix to postfix notation by repeatedly applying the
following rules.  It makes use of a temporary stack of tokens.

$\newcommand{\opin}{\mathit{op}_{\mathit{in}}}$
$\newcommand{\opstk}{\mathit{op}_{\mathit{stk}}}$

1. If the input is empty, move all operators on the temporary stack to
   the output.

2. If the next input token is a literal number, move it to the output.

3. If the next input token is a left parenthesis, push it onto the
   stack.

4. If the next input token is a right parenthesis:

    * If the top of the stack is a left parenthesis, discard both
      parens.

    * Otherwise, pop the top token from the stack and move it to the
      output (keeping the right paren in the input).

    This has the effect of moving all operators on the stack to the
    output up until the first matching left parenthesis.

5. If the next input token is an operator $\mathit{op}_{\mathit{in}}$:

    * If the stack is empty, push $\mathit{op}_{\mathit{in}}$ on the stack.
    * If the top of the stack is a token $\mathit{op}_{\mathit{stk}}$:

        * If the precedence of $\mathit{op}_{\mathit{in}}$ is $\leq$ the precedence of
          $\mathit{op}_{\mathit{stk}}$, pop $\mathit{op}_{\mathit{stk}}$ from the stack to the output and keep $\mathit{op}_{\mathit{in}}$
          in the input.

        * Otherwise, keep $\mathit{op}_{\mathit{stk}}$ on the stack and push $\mathit{op}_{\mathit{in}}$ on
          top of it.

    More concisely, when we see $\mathit{op}_{\mathit{in}}$ we pop operators from the
    stack as long as they have precedence greater than $\mathit{op}_{\mathit{in}}$, until
    either the stack is empty or the top operator on the stack has
    precedence less than $\mathit{op}_{\mathit{in}}$; then we push $\mathit{op}_{\mathit{in}}$.

* Write a function `shunt :: [Token] -> [Token]` which converts an
  infix expression into postfix, implementing the shunting yard
  algorithm discussed in class.

    * *Hint 1*: make another "helper" function with type `[Token] ->
      [Token] -> [Token]`, which keeps track of both the input list of
      tokens as well as the temporary operator stack.

    * *Hint 2*: make a function of type `Token -> Int` which returns
      the precedence of each token.  Be sure to give parentheses the
      *lowest* precedence (this means Rule 5 above does not need any
      special cases for parentheses). It probably doesn't matter what
      precedence you give to number literals, but logically they
      should have the *highest* precedence.

> precT :: Token -> Precedence
> precT RParen     = 0
> precT LParen     = 0 
> precT TPlus  = 1
> precT TMinus = 1
> precT TTimes = 2
> precT (TLit _)    = 3

> shunt :: [Token] -> [Token]
> shunt = shuntSupreme []

> shuntSupreme :: [Token] -> [Token] -> [Token]
> shuntSupreme stk [] = stk
> shuntSupreme stk (TLit n : ts)              = TLit n : shuntSupreme stk ts
> shuntSupreme stk (LParen : ts)             = shuntSupreme (LParen : stk) ts
> shuntSupreme (LParen : stk) (RParen : ts) = shuntSupreme stk ts
> shuntSupreme (op : stk) (RParen : ts)      = op : shuntSupreme stk (RParen : ts)
> shuntSupreme [] (opToken : ts)               = shuntSupreme [opToken] ts
> shuntSupreme (topOp : stk) (opToken : ts)
>     | precT topOp >= precT opToken = topOp : shuntSupreme stk (opToken : ts)
>     | otherwise = shuntSupreme (opToken : topOp : stk) ts

* Where do you think the given algorithm would need to be modified to
  accommodate both left- and right-associative operators?

  when its being pushed onto the stack, here:
  shuntSupreme (topOp : stk) (TOp op : ts)
     | prec topOp >= prec (TOp op) = topOp : shuntSupreme stk (TOp op : ts)
     | otherwise = shuntSupreme (TOp op : topOp : stk) ts

  the protocol for pushing and removing operators from the stack would need to be
  modified to allow for both associativities. This can be done by adding the case
  of an operator having the same precedence but being left-associative being popped
  from the stack instead of pushing the next one.