Project 1: Arith compiler
=========================

We won't spend much time in this course talking about compilers.  But
for this first project you will explore a very simple compiler for the
Arith language.

Preliminaries
-------------

First, download the files you will need:

* [ArithCompiler.lhs](ArithCompiler.lhs)
* [Parsing.hs](../../code/Parsing.hs)

`ArithCompiler.lhs` is the file you will edit for your project.  You
don't need to worry about `Parsing.hs`; just download it and put it in
the same directory as `ArithCompiler.lhs`.  You should be able to use
`repl.it` if you wish.  Just upload this file along with `Parsing.hs`
to your `repl.it` project and `:load ArithCompiler.lhs` as usual.

If you are using Haskell installed on your own computer, note that
depending on what version you have, you may need to start `ghci` with
a flag to tell it to use the `parsec` library, like so:

    ghci -package parsec ArithCompiler.lhs

First, some extensions and imports we will need for the parser; you
don't need to worry about these.

> {-# LANGUAGE GADTs #-}
>
> import Prelude hiding ((<$>), (<$), (<*>), (<*), (*>))
> import Parsing
  
> {-# OPTIONS_GHC -Wall #-}

AST and interpreter
-------------------

Here are the data types we used to represent Arith abstract syntax in
class, along with a simple interpreter.

> data Op where
>     Plus  :: Op
>     Minus :: Op
>     Times :: Op
>     deriving (Show, Eq)
>
> data Arith where
>     Lit :: Integer -> Arith
>     Bin :: Op -> Arith -> Arith -> Arith
>     deriving (Show)
> 
> interp :: Arith -> Integer
> interp (Lit n)        = n
> interp (Bin op a1 a2) = interpOp op (interp a1) (interp a2)
>
> interpOp :: Op -> Integer -> Integer -> Integer
> interpOp Plus  = (+)
> interpOp Minus = (-)
> interpOp Times = (*)

A parser has been provided for your convenience, to help you test your
functions.  You can use the `readArith` function to parse concrete
Arith syntax into an AST. Caution: DO NOT USE `readArith` for
anything besides testing at the ghci prompt, since it crashes (via
the `error` function) when given any `String` that does not parse.

> readArith :: String -> Arith
> readArith s = case parse parseArith s of
>     Left  err -> error (show err)
>     Right a   -> a

For example, try evaluating `interp (readArith "(2+3)*4")`, which
should result in 20. This is much more convenient than typing `interp (Bin Times
(Bin Plus (Lit 2) (Lit 3)) (Lit 4))`.

The abstract stack machine
--------------------------

Instead of compiling Arith programs to machine code, you will compile
them to an *abstract machine*.  An abstract machine is just like a
real machine except for the fact that it is imaginary.

Our imaginary machine is quite simple.  It keeps track of a list of
instructions to execute, and a stack of integers (recall that Haskell
lists can also be used as stacks).  There are four instructions it
knows how to execute:

+ `PUSH n`: given an integer `n`, push it on top of the stack.
+ `ADD`: pop the top two integers off the stack, add them, and push
  the result back on top of the stack.  The machine halts with an
  error if there are fewer than two integers on the stack.
+ `SUB`: pop the top two integers, subtract the topmost from the
  other, and push the result.
+ `MUL`: pop the top two integers, multiply them, and push the result.


1. **Make a data type called `Instruction` to represent the four stack
    machine instructions described above.**

> {- The data type `Instruction` has members representing the four instructions
> described above. `PUSH takes an integer and all the other memebers take no inputs.
> -}
> data Instruction where
>     PUSH :: Integer -> Instruction
>     ADD  :: Instruction
>     SUB  :: Instruction
>     MUL  :: Instruction
>     deriving Show

Our machine can also be in one of three states.  Each state may
additionally store some information.

+ `WORKING`: this state corresponds to normal operation of the
  machine.  It should contain a list of remaining instructions to
  execute and a stack of integers.

+ `DONE`: this state means there are no more instructions to execute.
  It should contain only the final stack.

+ `ERROR`: something has gone terribly, horribly wrong. In this state,
  the machine does not need to remember any instructions or stack.

2. **Make a data type called `MachineState` to represent the possible
   states of the machine, as described above.  Each different state
   should contain whatever information the machine needs to remember
   in that state.**

> {- The data type `MachineState` has three different states. `WORKING` takes a list
> of instructions and and a list of Integers, `DONE` takes a list of Integers, and
> `ERROR` takes not inputs.
> -}
> data MachineState where
>     WORKING :: [Instruction] -> [Integer] -> MachineState
>     DONE    :: [Integer] -> MachineState
>     ERROR   :: MachineState

3. **Write a function `step :: MachineState -> MachineState` which
   executes a single step of the machine.  For example, in the
   `WORKING` state it should try executing the next instruction and
   return an appropriate next state for the machine.**

> {- The step function takes a MachineState and returns the next MachineState by 
> processing all of the `WORKING` and `DONE` cases and then returning `ERROR` otherwise.
> -}
> step :: MachineState -> MachineState
> step (WORKING [] stack)               = DONE stack
> step (WORKING ((PUSH n): ins) stack)  = WORKING ins (n:stack)
> step (WORKING (ADD: ins) (n:m:stack)) = WORKING ins (n+m:stack)
> step (WORKING (SUB: ins) (n:m:stack)) = WORKING ins (n-m:stack)
> step (WORKING (MUL: ins) (n:m:stack)) = WORKING ins (n*m:stack)
> step _                                = ERROR


4. **Write `execute :: [Instruction] -> MachineState`, which takes a
   program and runs the machine (starting with an empty stack) until
   the machine won't run anymore (that is, it has reached a `DONE` or
   `ERROR` state).  (Hint: first write a helper function `steps ::
   MachineState -> MachineState`.)**

> -- The steps function determines whether to complete a new step or finish executing.
> steps :: MachineState -> MachineState
> steps workingState = 
>     case step workingState of
>         ERROR               -> ERROR
>         (DONE stack)        -> DONE stack
>         newState            -> steps newState

> -- execute simply calls the steps function.
> execute :: [Instruction] -> MachineState
> execute ins = steps (WORKING ins [])

5. **Finally, write `run :: [Instruction] -> Maybe Integer`, which
   executes the program and then returns `Nothing` if the machine
   halted with an `ERROR` or an empty stack, or `Just` the top integer
   on the stack if the machine successfully finished and left at least
   one integer on the stack.**

> {- The run function runs the execute and converts the `MachineState` result into a `Maybe
> Integer` depending on whether the result was an `ERROR` or `DONE`.
> -}
> run :: [Instruction] -> Maybe Integer
> run ins =
>     case execute ins of
>         ERROR            -> Nothing
>         (DONE [])        -> Nothing
>         (DONE (n:stack)) -> Just n


The compiler
------------

Now that you have a working abstract machine, you can compile Arith
expressions into equivalent programs that run on the abstract machine.

6. **Write a function `compile` which takes an `Arith` and yields a
list of `Instruction`s.**

> -- The `compile` function calls `compileHelper` with an extra argument.
> compile :: Arith -> [Instruction]
> compile arith = compileHelper arith []

> {- `compileHelper` takes an `Arith` and a list of `Instruction`s and returns
> the list of `Instruction`s with the `Instruction` generated from the topmost level
> of the Arith appended onto the front of it. It uses the helper function `compileOp`
> to handle operators in `Bin` Ariths, and passes `Lit` Ariths directly to the list as a
> push instruction.
> -}
> compileHelper :: Arith -> [Instruction] -> [Instruction]
> compileHelper (Lit n) ins = PUSH n : ins
> compileHelper (Bin op a1 a2) ins = compileHelper a2 [] ++ 
>       compileHelper a1 [] ++ [compileOp op] ++ ins

> -- `compileOp` replaces an `Op` with the corresponding `Instruction`.
> compileOp :: Op -> Instruction
> compileOp Plus  = ADD
> compileOp Minus = SUB
> compileOp Times = MUL

Of course, your compiler should output not just *any* list of
instructions!  It should output a program which, when run on the
abstract machine, successfully produces the same integer result as the
Arith interpreter would.  That is, for any `a :: Arith`,

```
run (compile a) == Just (interp a)
```

To test the above it will be convenient to write a function which
finally puts some of these things together:

7. **Write a function `exec :: String -> Maybe Integer` which takes a
   `String`, parses it, compiles the resulting `Arith`, and then
   `run`s the generated abstract machine instructions.  It should
   return `Nothing` if the given `String` does not parse.** (Do not
   use `readArith`!)

> {-  The exec function makes use of the `parseArith` parser and adds error handling
> using `Maybe`. Originally I misunderstood the instructions and wrote a pareser, but it
> didn't handle whitespaces and certain cases with negative numbers and parenthesis.
> once I figured out that I didn't have to do it I scrapped it entirely.
> -}
> exec :: String -> Maybe Integer
> exec s = case parse parseArith s of
>     Left  err -> Nothing
>     Right a   -> run (compile a)

You should now be able to test that if `s` is any `String`, then `eval
s == exec s`.

Level 1
-------

To complete this project to Level 1, do the above steps completely and
correctly. `eval s == exec s` must be true for all strings `s`.

Level 2
-------

To complete this project to Level 2, in addition to the requirements for Level 1:

- Ensure that your code uses [good Haskell
   style](https://kowainik.github.io/posts/2019-02-06-style-guide),
   for example:
    - Use `camelCase` for variable names.
    - Use informative but not-too-long variable names.
    - Vertically align `=` signs within a function definition.
    - Use consistent indentation (see above linked style guide).
        - Note in particular "The indentation of a line should not
           depend on the length of any identifier in preceding
           lines".

- Make sure your code is simplified as much as possible, for example,
  without redundant pattern-matching.

- Turn on `{-# OPTIONS_GHC -Wall #-}` and make sure your code generates no warnings.

- Write informative, grammatically correct comments explaining your
   code, its operation, and any choices you made along with the
   reasons for those choices.

Level 3
-------

Consider a different virtual machine with the following
characteristics:

- The machine has a (potentially infinite) memory of integer cells,
  addressed by consecutive natural numbers 0, 1, 2, ... The cells all
  start out with value 0.
- It also has a single "accumulator register" which can store a
  single integer.  The accumulator also starts as 0.
- The machine supports the following instructions:
    - `ACCUM n`: Set the value of the accumulator to the integer `n`.
    - `STORE a`: Store the current contents of the accumulator into the memory
      cell with address `a`.
    - `ADD a`: Add the contents of the memory cell at address `a` to
      the accumulator, updating the accumulator with the new value.
    - `SUB a`: Subtract the contents of the memory cell at address `a`
      from the accumulator, updating the accumulator with the new value.
    - `MUL a`: Multiply the contents of the memory cell at address `a`
      by the accumulator, updating the accumulator with the new value.

You should implement:

- A `run` function which executes a list of instructions for this
  abstract machine, and returns the final contents of the accumulator.
- A compiler which takes an `Arith` expression and compiles it to a
  list of instructions for this virtual machine.
- Be sure to include some examples showing that your compiler works
  correctly, that is, that your compiler is semantics-preserving:
  interpreting an `Arith` expression should give the same result as
  compiling and then running.

> {- `Instruction2` works effectively the same way as `Instruction` but uses the
> instructions from the new abstract machine.
> -}
> data Instruction2 where
>     Accum :: Integer -> Instruction2
>     Store :: Integer -> Instruction2
>     Add   :: Integer -> Instruction2
>     Sub   :: Integer -> Instruction2
>     Mul   :: Integer -> Instruction2
>     deriving Show

> -- The cell datatype is effectively a list of `Integer`s
> type Cell = [Integer]

> {- I decided to create functions to store and fetch `Integer`s from the `Cell` list.
> The `storeInt` function recursively goes through the list of `Cell`s and creates new `Cell`s
> as it goes, initializing them with a value of zero. When it reaches the target it 
> sets the new value and returns the new `Cell` list. If it is given a negative address
> it returns `Nothing` instead.Assoc
> -}
> storeInt :: Integer -> Integer -> Cell -> Maybe Cell
> storeInt n address []      = storeInt n address [0]
> storeInt n 0 (c:rst)       = Just (n:rst)
> storeInt n address (c:rst) = case storeInt n (address - 1) rst of
>     Just cell -> if address > 0 then Just (c:cell) else Nothing
>     Nothing   -> Nothing

> {- `fetchInt` recursively searches through the list until it reaches the target
> and returns the value in that `Cell`. If the `Cell` has not been created it returns
> 0 because it would have been created with a value of 0.
> -}
> fetchInt :: Integer -> Cell -> Integer
> fetchInt address []      = 0
> fetchInt 0 (c:rst)       = c
> fetchInt address (c:rst) = fetchInt (address - 1) rst

> {- `run2` does the same thing as `execute`, and calls a helper function with more 
> arguments
> -}
> run2 :: [Instruction2] -> Maybe Integer
> run2 ins = runHelper ins 0 []

> {- `runHelper` is responsible for the processing of the `[Instruction2]`. It does
> this by using a case to check the various `Instruction2`s and then doing the required
> operation. 
> -}
> runHelper :: [Instruction2] -> Integer -> Cell -> Maybe Integer
> runHelper [] accum _      = Just accum
> runHelper (i:ins) accum c = case i of
>     Accum n -> runHelper ins n c
>     Store n -> case storeInt accum n c of
>         Just cell -> runHelper ins accum cell
>         Nothing   -> Nothing
>     Add n   -> runHelper ins (fetchInt n c + accum) c
>     Sub n   -> runHelper ins (accum - fetchInt n c) c
>     Mul n   -> runHelper ins (accum * fetchInt n c) c

> -- `compile2` calls `compileHelper2` to convert `Arith`s to `[Instruction2]`.
> compile2 :: Arith -> [Instruction2]
> compile2 a = case compileHelper2 a 0 of
>     (ins, _) -> ins

> {- `compileHelper2` takes and Arith and the current `Cell` address and returns
> the `[Instruction2]` and the new `Cell` address. It returns the address so that
> it could be called multiple times when disassembling a `Bin` without colliding with
> the address used by previous operations.
> -}
> compileHelper2 :: Arith -> Integer -> ([Instruction2], Integer)
> compileHelper2 (Lit n) address = ([Accum n], address)
> compileHelper2 (Bin op a1 a2) address =
>     case compileHelper2 a1 address of
>         (ins1, address1) -> case compileHelper2 a2 address1 of
>             (ins2, address2) -> (ins2 ++ [Store address2] ++ ins1 ++ [compileOp2 op address2], address2 + 1)

> -- `compileOp2` converts operators to abstract machine `Instruction2`s.
> compileOp2 :: Op -> Integer -> Instruction2
> compileOp2 Plus n  = Add n
> compileOp2 Minus n = Sub n
> compileOp2 Times n = Mul n

> {- This was a testing function to make sure the `[Instruction2]` was being built
> properly.
> -}
> insTest :: String -> [Instruction2]
> insTest s = case parse parseArith s of
>     Left  err -> error (show err)
>     Right a   -> compile2 a

> {- The `exec2` function uses the parser parseArith to parse the `String`, then compile
> and run the result.
> -}
> exec2 :: String -> Maybe Integer
> exec2 s = case parse parseArith s of
>     Left  err -> Nothing
>     Right a   -> run2 (compile2 a)

Parser
------

[Pay no attention to the man behind the curtain](https://www.youtube.com/watch?v=YWyCCJ6B2WE)...

> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>
> parens :: Parser a -> Parser a
> parens = getParens lexer
>
> reservedOp :: String -> Parser ()
> reservedOp = getReservedOp lexer
>
> integer :: Parser Integer
> integer = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> parseAtom :: Parser Arith
> parseAtom = Lit <$> integer <|> parens parseExpr
>
> parseExpr :: Parser Arith
> parseExpr = buildExpressionParser table parseAtom
>   where
>     -- Each list of operators in the table has the same precedence, and
>     -- the lists are ordered from highest precedence to lowest.  So
>     -- in this case '*' has the highest precedence, and then "+" and
>     -- "-" have lower (but equal) precedence.
>     table = [ [ binary "*" (Bin Times) AssocLeft ]
>             , [ binary "+" (Bin Plus)  AssocLeft
>               , binary "-" (Bin Minus) AssocLeft
>               ]
>             ]
>
>     binary name fun assoc = Infix (reservedOp name >> return fun) assoc
>
> parseArith :: Parser Arith
> parseArith = whiteSpace *> parseExpr <* eof
>
> eval :: String -> Maybe Integer
> eval s = case parse parseArith s of
>            Left _  -> Nothing
>            Right a -> Just (interp a)
