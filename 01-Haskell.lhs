Module 01: Introduction to Haskell
==================================

In this module you will focus on learning some of the basics of the
Haskell programming language.  If you already know some Haskell, you
should focus on helping your partner(s) understand all the material in
this module.  However, keep in mind that people learn best by doing,
not by being told.  **The driver should be whoever has the *least*
experience with Haskell.**

This file is a "literate Haskell document": only lines preceded by >
and a space (see below) are code; everything else (like this
paragraph) is a comment, formatted using Markdown syntax.  Literate
Haskell documents have an extension of `.lhs`, whereas non-literate
Haskell source files use `.hs`.

GHCi
----

* Run `ghci` from a command prompt (or from the "Shell" tab in
  `repl.it`).  At the resulting prompt, type `:help`.

* Find the command to exit `ghci`.  What is it?  Exit `ghci` now.

:quit

* Using the `cd` command at the command prompt, change to the directory
  containing this file, `01-Haskell.lhs` (if using `repl.it`, you
  should already be in the correct directory).

* Now start `ghci` again.  Find the command to load a module.  What is
  it?  Load this file, `01-Haskell.lhs`, into `ghci`.  Note that by
  default, `ghci` will look for files in the same directory in which it
  was started.

Hint: to kill a runaway `ghci` evaluation, use Ctrl+C.

Basic Haskell declarations
--------------------------

Now consider the following Haskell code.

> i :: Int
> i = -35
>
> n :: Integer
> n = 25
>
> c :: Char
> c = 'Z'
>
> b :: Bool
> b = True
>
> s :: String
> s = "Hello, world!"
>
> f :: Integer -> Integer
> f n = 2*n + 1
>
> g :: Integer -> Integer -> Integer
> g m n = (m - n)*(m + n)
>
> -- This is a comment
> {- So
>    is
>    this -}
>
> -- Uncomment me:
> -- i = 12

* Enter `:type n` at the `ghci` prompt.  What does the `:type` command
  do?

Tells us the type of a variable.

* What do you think `::` means?

Tells us what type something has.

* What do you think `=` means?

Assignment of the value of a variable.

* What do you think `->` means?

Output type for a function.

* Find the `ghci` command to reload the current module.  Uncomment the
  line `i = 12` above, save this file, and reload.  What happens?

  It fails with the error multiple declarations of i.

* Does this change your answer to the question about what `=` means?

Changes it to the inital declaration of the value.

Arithmetic
----------

* At the `ghci` prompt, type each of the following expressions, and
record the result.  Feel free to experiment with other expressions as
well.

    ```{.haskell}
    3 + 2
    19 - 27
    div 19 3
    19 `div` 3
    mod 19 3
    19 `mod` 3
    19 `divMod` 3
    7 ^ 222
    (-3) * (-7)
    2*i + 3
    i + n
    ```

* Explain what happens when you evaluate `i + n`.

Couldn't match expected type with Int with actual type Integer. Why are Int and Integer different?

* What are the smallest and largest possible `Int` values?

-9223372036854775808 and 9223372036854775807 see https://stackoverflow.com/questions/42472790/minbound-and-maxbound-in-haskell

* What are the smallest and largest possible `Integer` values?

There are no min and max bound for Integer

(Haskell has floating-point values too, but we won't use them much in
this course.)

Booleans
--------

* Find out the syntax for each of the following operations in Haskell:

    - Boolean operations: and, or, not

    &&, ||, not see https://www.cantab.net/users/antoni.diller/haskell/units/unit01.html

    - Comparison: equal, not equal, less than, greater than, less or
      equal, greater or equal

    ==, /=, <, >, <=, >=

    - if-expressions

    if <Condition> then <value> else <value> see https://www.tutorialspoint.com/haskell/haskell_if_else_statement.htm

  Of course, be sure to cite any resources you use!


* Play around with the operators you discovered and try them on some
  examples. Record three of your most interesting experiments,
  the result, and what you learned from each.

  Haskell does not allow the use of one and zero in place of true and false values. Tested by trying 1 && 1 and 0 && True

  Tested whether Haskell allowed comparisons between Int and Integer by creating an Int and and Integer with the same value and comparing them. They were comparable.

  Tested to see if the if statement syntax we found was correct by building the if statement if a /= 3 then "yes" else "no", where a == 5. It evaluated to "yes", so we think it works.





Pairs
-----

* Type `(n,c)` at the `ghci` prompt.  What is the result?

(25,'Z')

* What is the type of `(n,c)`?

(Integer, Char)

* What is the result of `fst (n,c)`?

returns 25

* What is the result of `snd (n,c)`?

'Z'

* What is the type of `fst`?  What does it do?

the type is (a, b) -> a. It returns the first item in a pair.

Values like `(n,c)` are called *pairs*, or more generally,
*tuples*. (Haskell also has 3-tuples, 4-tuples, ... but we will not
use them.)


* Define `e` such that `fst (fst (snd (fst e))) == 6`.

e = ((7, ((6, 7), 7)), 7)

Functions
---------

Evaluate the following expressions:

```{.haskell}
f 6
f 8
g 5 4
g 2 3
```

A *function* takes one or more input values and produces a single
output value.

* What is the Haskell syntax for applying a function to a single
  argument?

  function argument

* What is the Haskell syntax for applying a function to multiple
  arguments?

  function argument argument argument ...

* Write a function which takes two `Integer` values as input and
  returns `True` if and only if the first is greater than twice the
  second.  What is the type of your function?

h :: Integer -> Integer -> Bool

h m n = m > 2 * n

Pattern matching
----------------

> wub :: Integer -> Integer
> wub 0 = 1
> wub n = n * wub (n-1)
>
> dub :: Integer -> Integer
> dub 1 = 1
> dub 0 = 0
> dub n = dub (n-1) + dub (n-2)
>
> flub :: (Integer, Integer) -> Integer
> flub p = fst p + 2 * snd p
>
> gub :: (Integer, Integer) -> Integer
> gub (x,y) = x + 2*y

* Evaluate `wub 0`, `wub 1`, and `wub 5`.

1, 1, 120

* Explain in words what `wub` does.

It recursively calculates the factorial of n.

* What does the line `wub 0 = 1` mean?

It sets the output for an input of 0 equal to 1. The base case.

* What do you think would happen if the lines `wub 0 = 1` and
  `wub n = n * wub (n-1)` were switched? Make a guess *before* trying it, and
  record your guess here.

  JC guesses that it will go forever. Ted thinks that it will always return 0. 

* Now try it.  What happens?  Why?

It immediatly threw and error saying it was redundant. JC was correct. 
It ran infinitely until the stack overflowed. Because there was no base case it did not know when to stop.

* What happens when you evaluate `wub (-3)`?  Why?

It ran until stack overflow because it recursed away from the base case and did not know when to stop.

* Evaluate `wub (3+1)` and `wub 3+1`.  Can you explain the
  difference?

  It evaluates the function before the addition without the parenthises.

* What does `dub` do?

Dub recursively generates the fibonacci sequence.

* What happens if the lines `dub 0 = 0` and `dub 1 = 1` are switched?

It doesn't do anything different because neither are functions they are both set values. As long as they come before the function it will work the same.

* Call `flub` and `gub` on some example inputs.  Record your results
  here.  Do you notice a difference between the behavior of `flub` and
  `gub`?

(2, 3) -> 8, 8
(3, 3) -> 9, 9
(-3, 3) -> 3, 3
(0, 15) -> 30, 30

No we don't notice a differnece in behavior between flub and gub.

* Explain the difference between `flub` and `gub`.

The difference is that flub takes in a pair and uses fst and snd to diferentiate the values in the pair. 
Gub takes the values of the pair as differenct parameters and does not use other functions.

* Which do you prefer?  Why?

We prefer gub because it does not depend on other functions and is easier to understand.

Guards
------

> hailstone :: Integer -> Integer
> hailstone n
>   | even n    = n `div` 2
>   | otherwise = 3*n + 1

* Try evaluating `hailstone` on some example inputs; record them here.

5 -> 16
6 -> 3
7 -> 22
8 -> 4

* Try evaluating `even` on some example inputs.  What does the `even`
  function do?

even returns a true or false value depending on whether an input is even or odd.

* How is `otherwise` defined? (You'll have to Google this one.)

Otherwise is defined as otherwise = True. It always evaluates to True in guards. see http://learnyouahaskell.com/syntax-in-functions

* Explain the behavior of `hailstone`.

divide n by 2 if n is even, otherwise multiply n by 3 and add 1.

Practice
--------

* Write a function `inRange` which takes two inputs, a pair of
  `Integer`s and an `Integer`, and checks whether the `Integer` is in
  between the elements of the pair (inclusive).  For example,
  `inRange (2,4) 2`, `inRange (2,4) 3`, and `inRange (2,4) 4` should all be
  `True`, whereas `inRange (2,4) 6` should be `False`.  Note that
  `inRange (4,2) 3` should also be `True`.

> inRange :: (Integer, Integer) -> Integer -> Bool
> inRange (a,b) c
>   | a <= b     = c >= a && c <= b
>   | otherwise = c <= a && c >= b
