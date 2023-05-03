Module 04: Syntax and semantics
===============================

* Write your team names here: Ted Bjurlin, Taylor Aishman, Dalton Casey

* You may again choose whoever you want to start as the driver.  Write
  your choice here:

  Ted Bjurlin

**Remember**:

+ Be sure that your module loads into GHCi with no errors before
  turning it in.
+ Write in complete sentences, with capital letters and punctuation.
  Presentation matters!

> {-# LANGUAGE GADTSyntax #-}
>
> import Data.Char   -- so you can use the 'isDigit' function later

Some example grammars
---------------------

**Example 1**

    <mirror> ::= '.'
               | 'L' <mirror> 'R'

Example strings that match `<mirror>`:

    "."
    "L.R"
    "LL.RR"

Example strings that do not match `<mirror>`:

    "Q"
    "LR"
    "LL.RRR"

**Example 2**

    <tree> ::= '#'
             | '(' <tree> ')'
             | '(' <tree> <tree> ')'

Example strings that match `<tree>`:

    "(##)"
    "((#)(#(##)))"

Example strings that do not match `<tree>`:

    "((#)"
    "(###)"

**Example 3**

    <digit>   ::= '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    <zero>    ::= '0'
    <natural> ::= <zero> | <digit> | <digit> <zero>* <natural>
    <integer> ::= <natural> | '-' <natural>

Example strings that match `<integer>`:

    "0023"
    "-25"

Example strings that do not match `<integer>`:

    "x27"
    "--25"
    "4-2"

* For each of `<mirror>`, `<tree>`, and `<integer>`, give three more
  examples of strings that match, and three more examples that do not
  match.

  LLL.RRR, LLLL.RRRR, LLLLL. RRRRR all match mirror. L.RRRRR, LLLLLL.R, RRR all
  do not match mirror.

* What does `|` mean?  (Note for this question and the next: this is
  **not** Haskell syntax!  Just say what you think these notations mean
  based on the examples above.)

  It is an 'or' operator.

* What is the difference between something in single quotes (like '*')
  and something in angle brackets (like `<tree>`)?

  Something in single quotes has to match the exact character, while the
  brackets has to match the type.

The things in single quotes are usually called *terminals*, and the
things in brackets are called *nonterminals*.  These sorts of definitions are
known as (context-free) *grammars*, written in **Backus-Naur Form** or
**Backus Normal Form** (BNF), named for John Backus and Peter Naur.

* In what context was BNF first developed?

BNF was developed to describe the syntax of ALGOL 58.

* What else are John Backus and Peter Naur known for?

John Backus directed the team that developed FORTRAN, and Peter Naur is known
for his work on ALGOL 60, as well as winning a Turing Award

Now, back to your regularly scheduled grammars...

* Does `<natural>` match the empty string ""?  Why or why not?

No because it a <natural> can only be a <digit> or a <digit> and a <natural>,
and <digit> does not match a "".

* An alternative, equivalent way to define `<natural>` is as follows:

        <natural> ::= <digit>+

    Given that this is equivalent to the original definition, what do
    you think `+` means?

The plus indicates that it can have any number of <digit>s.

Technically this sort of `+` notation was not included in the original
form of BNF, but it is a common extension.

* `<natural> ::= <digit>*` would match all the same strings as
  `<digit>+`, but _also_ matches the empty string.  What do you
  think `*` means in this context?

The plus indicates that it can have any nonzero natural number of <digit>s,
while the '*' allows zero <digit>s as well.

* Describe how to modify the definition of `<natural>` so it does not
  allow unnecessary leading zeroes.  That is, `"203"` should still match
  but `"0023"` should not; however, `"0"` should still be a valid
  `<natural>`. If you wish, you can also introduce more definitions or
  modify definitions besides `<natural>`.

By adding zero as a seperate case then only allowing zeros by themselves,
or any number of zeros in between a digit and an natural.

* Write down a grammar (as concisely as possible) that matches all these
  strings:

        "(XX)Y"
        "(XXXX)YZ"
        "(XX)ZZZZZ"
        "(XXXXXX)YZZ"

    ...but does not match any of these strings:

        "()Y"
        "(XXX)YZ"
        "(X)ZZZ"
        "(XX)YY"

    <xyz> ::= '(' 'XX'+ ')' 'Y'? 'Z'*

![](../images/stop.gif)

Mirror, mirror
--------------

* **ROTATE ROLES** and write the name of the new driver here: Taylor Aishman

> {-
>    <mirror> ::= '.'
>               | 'L' <mirror> 'R'
> -}
>
> data Mirror where
>   Middle :: Mirror
>   Layer  :: Mirror -> Mirror
>   deriving Show
>
> prettyMirror :: Mirror -> String
> prettyMirror Middle    = "."
> prettyMirror (Layer m) = "L" ++ prettyMirror m ++ "R"
>
> parseMirror :: String -> (Mirror, String)
> parseMirror ('.' : rest) = (Middle, rest)
> parseMirror ('L' : rest) =
>   case parseMirror rest of
>     (m, 'R' : rest') -> (Layer m, rest')

* Write down three different example values of type `Mirror`.
Layer Middle
Layer (Layer Middle)
Layer (Layer(Layer Middle))

* Try calling `prettyMirror` on your example `Mirror` values above, and
  record the results.
"L.R"
"LL.RR"
"LLL.RRR"

* For this language, how are the concrete syntax (represented by the
  grammar `<mirror>`) and abstract syntax (represented by the data type
  `Mirror`) different?
  <mirror> operates off the BNF which in theory is the universal syntax programming languages are built off of
  Mirror is specific to Haskell and is interpreted by it through its built in functions and can't be understood
  by BNF

* Try calling `parseMirror` on five different example inputs.  An
  example input can be *any* `String`.  Try to pick a variety of
  examples that show the range of behavior of `parseMirror`.  Record
  the results here.
"LL.RR" converts to (Layer (Layer Middle),"")
"LLL.RRR" converts to (Layer (Layer (Layer Middle)),"")
"hellow world" converts to *** Exception: 04-syntax-semantics.lhs:(183,3)-(186,42): Non-exhaustive patterns in function parseMirror
"12345" converts to *** Exception: 04-syntax-semantics.lhs:(183,3)-(186,42): Non-exhaustive patterns in function parseMirror
"" converts to *** Exception: 04-syntax-semantics.lhs:(183,3)-(186,42): Non-exhaustive patterns in function parseMirror

* Describe the behavior of `parseMirror`.  Your answer should refer to
  the grammar `<mirror>`.
  It converts from <mirror> to Mirror by recursively calling itself through the <mirror> until it hits the '.'
  All 'L' are converted to a Layer while the '.' is converted to a Middle
  Throws an exception if it doesn't fit that grammar 

* Why does `parseMirror` return a `(Mirror, String)` pair instead of
  just a `Mirror`? (*Hint*: if you are not sure, try writing a function
  `parseMirror2 :: String -> Mirror` which behaves the same as
  `parseMirror` but does not return the extra `String`.)
  To check if there is the same number of 'L' and 'R' you have to keep the string through the whole function
  By keeping the string around it is just easier to return it in case there is an error 

* Modify `parseMirror` so that it has type `String -> Maybe (Mirror,
  String)` and never crashes.  Instead of crashing on inputs that do not
  match `<mirror>`, it should return `Nothing`.  Call your modified
  function `parseMirrorSafe` and write it below.

```{.haskell}
data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a
```

> parseMirrorSafe :: String -> Maybe (Mirror, String) 
> parseMirrorSafe ('.' : rest) = Just (Middle, rest)
> parseMirrorSafe ('L' : rest) =
>   case parseMirrorSafe rest of
>     Just (m, 'R' : rest') -> Just (Layer m, rest')
> parseMirrrorSafe _ = Nothing

![](../images/stop.gif)

BIN (aka Your First Language)
------------------------------

* **ROTATE ROLES** and write the name of the new driver here: Dalton Casey

Consider the following BNF grammar:

    <bin> ::= '#'
            | '(' <bin> <bin> ')'

* Write an algebraic data type called `Bin` which corresponds to
  `<bin>`.  That is, `Bin` should encode the abstract syntax trees
  corresponding to the concrete syntax `<bin>`.

> data Bin where
>   Stuff :: Bin
>   Tree :: Bin -> Bin -> Bin
>   deriving Show


* Write a function `prettyBin` which turns an abstract syntax tree into
  concrete syntax.

> prettyBin :: Bin -> String
> prettyBin Stuff = "#"
> prettyBin (Tree b1 b2) = "(" ++ prettyBin b1 ++ prettyBin b2  ++ ")"



* Write a function `parseBin :: String -> Maybe (Bin, String)` which
  turns concrete syntax into abstract syntax.

> parseBin :: String -> Maybe (Bin, String)
> parseBin ('#' : rest) = Just(Stuff, rest)
> parseBin ('(' : rest) = 
>   case parseBin rest of
>     Nothing -> Nothing
>     Just(b1, rest') -> 
>       case parseBin rest' of
>         Just(b2, ')': rest'') -> Just(Tree b1 b2, rest'')
>         _ -> Nothing
> parseBin _ = Nothing



One way we could give a *semantics* (meaning) to abstract `Bin` trees
is as follows: a leaf has value 1; the value of a branch with left and right
subtrees is the value of the left subtree plus twice the value of the
right subtree.

* Write a function `interpBin :: Bin -> Integer` which implements this
  semantics.

> interpBin :: Bin -> Integer
> interpBin Stuff = 1
> interpBin (Tree b1 b2) = interpBin b1 * 2 + interpBin b2

We can think of this as a little programming language for computing
integers; let's call it BIN.  (Of course, BIN is not a very useful
language, but don't let it hear you say that because you might hurt
its feelings.)  For example, `"((##)(##))"` is a program to compute
the value 9.  `parseBin` is a *parser* for the language, and `interpBin`
is an *interpreter*.

* Put the pieces together to create a function `evalBin :: String ->
  Maybe Integer`. For example,

        evalBin "#"          --> Just 1
        evalBin "((##)(##))" --> Just 9
        evalBin "(##"        --> Nothing

> evalBin :: String -> Maybe Integer
> evalBin s = 
>   case parseBin s of
>     Just(b, _ ) -> Just(interpBin b)
>     Nothing -> Nothing


![](../images/green.png)

EBIN
----

* **ROTATE ROLES** and write the name of the new driver here:

The language EBIN (which stands, of course, for *extended* BIN) is
just like BIN except it also allows single digits, which can stand in
for an entire tree.  In other words, a digit can go anywhere a `#` can
go in a BIN program.  For example, `"((4#)(#2))"` is a valid EBIN
program.  Note that `"((##)(42))"` is also a valid EBIN program, which
does not contain the number 42 but rather the single digits `4` and
`2`.

The digit `n` stands for a full binary tree of height `n`.  For
example,

+ `0` stands for `#`
+ `1` stands for `(##)`
+ `2` stands for `((##)(##))`

and so on.  Thus, EBIN is not a completely new, separate language, but
just adds some *syntax sugar* to BIN.  That is, single digits are just
a convenient shorthand for more cumbersome but equivalent expressions
in BIN.  We define EBIN in terms of BIN, and we can think of EBIN
programs as abbreviated BIN programs.

* Write down a BNF grammar for EBIN.

* Make an algebraic data type `EBin` to represent EBIN abstract
  syntax. (Note, at this stage you should not be expanding digits into
  full binary trees; we will do that later.)  Just record the
  possibilities that EBIN expressions could contain a digit, a `#`, or a
  parenthesized pair of EBIN expressions.

* Write a parser for EBIN. (*Hints*: you may find the `isDigit` and
  `read` functions useful.  Note that `Data.Char` was imported at the
  beginning of the module so you can use `isDigit`.  You are always
  welcome to import other library modules as needed. `read` has a more
  general type but for the purposes of this assignment you can think
  of it as having type `String -> Integer`; for example, `read "34" =
  34`.)

* Write a function `desugar :: EBin -> Bin` which *desugars* EBIN
  abstract syntax into equivalent BIN abstract syntax.  (*Hint*: you
  will probably find it useful to write a separate function `grow ::
  Integer -> Bin` which converts an integer into a full tree of the
  given height.)

* Would it be possible to do the desugaring phase *before* the
  parsing phase?  If so, what would be the type of `desugar`?

* Now put all the pieces together to define an evaluator `evalEBin ::
  String -> Maybe Integer`.  That is, `evalEBin` should encompass the
  following phases:

    Concrete syntax $\rightarrow$ *parse* $\rightarrow$ EBIN AST $\rightarrow$ *desugar* $\rightarrow$ BIN AST $\rightarrow$ *interpret* $\rightarrow$ Integer

    For example, `evalEBin "(#2)"` should yield 19, and `"((4#)(#2))"`
    should evaluate to 121.

As an (optional) fun aside, make a conjecture about the value of EBIN
programs consisting of a single digit.  That is, what can you say
about `evalEBin "0"`, `evalEBin "1"`, `evalEBin "2"`, and so on?  Can
you prove your conjecture?
