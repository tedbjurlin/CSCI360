> {-# OPTIONS_GHC -Wall #-}

Module 02: Algebraic data types and pattern matching
====================================================

* Record your team members here: Ted Bjurlin and JC Ntambara

For this module, the person whose birthday is latest in the year
should start out as the driver.  The module will indicate points when
you should rotate roles.

**Remember**, you should make sure that everyone on your team is
understanding everything, regardless of their prior amount of Haskell
experience.

> {-# LANGUAGE GADTSyntax #-}

The above {-# LANGUAGE #-} thingy turns on a Haskell language
extension called "GADTSyntax" (GADT stands for "Generalized Algebraic
Data Type").  You need not worry about what that means for now; it
will enable us to use some nice syntax.

Enumerations
------------

> data Color where
>   Red   :: Color
>   Green :: Color
>   Blue  :: Color
>   deriving Show
>
> colorChar :: Color -> Char
> colorChar Red   = 'r'
> colorChar Green = 'g'
> colorChar Blue  = 'b'
>
> isRed :: Color -> Bool
> isRed Red   = True
> isRed _ = False

* Load this file into GHCi and type `Red` at the prompt.  What happens?

It just says Red.

* What is the type of `Red`?

Red :: Color

* What does the function `colorChar` do?

Returns the associated character for each color.

* What does the function `isRed` do?

returns true if the color is red, otherwise false

* The `data Color where ...` declaration defines an *algebraic data
  type* (ADT) called `Color`.  `Red`, `Green`, and `Blue` are called
  *data constructors*, or just *constructors* for short.  Explain what
  you think the relationship between an algebraic data type and its
  constructors is.

The constructors are members of the ADT. So Red, Green, and Blue are
members of the type Color.

* Try removing or commenting out the last line of the definition of
  `colorChar`.  Reload the module (by typing `:reload` or just `:r` at
  the GHCi prompt) and try evaluating `colorChar Blue`.  What happens?

We got an exception: Non-exhaustive patters in function colorChar. An
exception because the function cannot process all the members of the
type Color. We defined Blue and did not use it in the function.

* Now add `> {-# OPTIONS_GHC -Wall #-}` as the very first line of this
  file (with a blank line after it), and reload again.  Explain what
  happens.

It actually tells us the problem now: Patterns of type `Color` not matched: Blue.

* (If you wish you can now put `colorChar` back to the way it was at
  first.)

![](../images/stop.gif) **When you reach this point, STOP and let Dr. Yorgey know.**

More general ADTs
-----------------

**ROTATE ROLES**

> data MaybeInteger where
>   No  :: MaybeInteger
>   Yes :: Integer -> MaybeInteger
>   deriving Show
>
> mi1, mi2 :: MaybeInteger
> mi1 = No
> mi2 = Yes 6
>
> unMaybe :: MaybeInteger -> Integer
> unMaybe No = 0
> unMaybe (Yes 6) = 249
> unMaybe (Yes n) = n
>
> data Record where
>   NameAndAge      :: String -> Integer -> Record
>   AddressAndEmail :: String -> String -> Record
>   TopSecret       :: Integer -> Bool -> (Char, Integer) -> Record
>   deriving Show
>
> record1, record2, record3 :: Record
> record1 = NameAndAge "McGrew" 6
> record2 = AddressAndEmail "55 Ridge Avenue" "mcgrew@mcgrew.com"
> record3 = TopSecret 17 False ('x',10)
>
> recordAge :: Record -> Integer
> recordAge (NameAndAge _ age)          = age
> recordAge (AddressAndEmail _ _)       = 0
> recordAge (TopSecret age True _)      = age
> recordAge (TopSecret _ False (_,age)) = age
>
> recordAge2 :: Record -> Integer
> recordAge2 r =
>   case r of
>     (NameAndAge _ age)          -> age
>     (AddressAndEmail _ _)       -> 0
>     (TopSecret age True _)      -> age
>     (TopSecret _ False (_,age)) -> age
>
> foo :: Record -> Integer
> foo r = 3 * (case r of
>                 NameAndAge _ age -> age
>                 _                -> 7
>             )
>         + 2

* What is the type of `No`?  What is the type of `Yes`?
The type is -> No :: MaybeInteger, Yes -> Yes :: Integer -> MaybeInteger
* Explain in English what values of type `MaybeInteger` look like.
  (*Hint*: your answer should contain the word "either".)
The values could be either No or yes followed by an integer. 
* Go back and reread your answer to the question about the
  relationship between algebraic data types and constructors.  Has your
  answer changed at all?  If so, write down a revised version here.

Yes, the constructors act more like the class methods or variables. 

* What does `unMaybe (Yes 50)` evaluate to?  What about `unMaybe
  (Yes 6)`?

50, 249 respectivelly

* Try removing some parentheses from the definition of `unMaybe`,
  for example, change the middle line to `> unMaybe Yes 6 = 249`.
  Reload the module.  Can you explain the resulting error message?
  (You can then put `unMaybe` back as it was.)

Removing the parantheses makes unMaybe have more than argument while the parantheses keeps everything inside them as one argument.

* Write a function of type `MaybeInteger -> Integer` with the
  following behavior:

    - If there is no `Integer`, return 0
    - If there is an even `Integer`, return half of it
    - If there is an odd `Integer`, return double it

  You should write your function definition below, using bird tracks
  (greater-than signs) in front of your code, just like the rest of the
  code in this module.  Be sure to `:reload` the module in GHCi to test
  your code.

> evenOddInteger::MaybeInteger -> Integer
> evenOddInteger No = 0
> evenOddInteger (Yes n) = if even n then n `div` 2 else n*2

* Describe in English what values of type `Record` look like.
String Name and Integer Age, String Address and String Email, or Integer, boolean, and a pair of a character and integer (TopSecret).
* Look at the definition of `recordAge`.  What do you think `_` means?
In this case, it's like a place holder for something that won't be needed. 
  Predict the output of `recordAge` on the inputs `record1`,
  `record2`, and `record3`.

  Predictions: 6, 0, 10

* Evaluate `recordAge` on `record1`, `record2`, and `record3`.  Were
  you right?  If not, does it change what you think `_` means?
No, it matches our prediction and our thinking.

* The underscore `_` which can occur on the left-hand side of the `=`
  sign in a function definition is called a *wildcard*.  Can you go
  back and simplify the definition of the `isRed` function using a
  wildcard?  Why or why not?
Yes, because we could simply replace Green and Blue in  the False condition with the wildcard.

* Write a function of type `MaybeInteger -> Integer` which always
  returns 3, no matter what input it is given.  Make your function
  definition as simple as possible.

> maybeThree :: MaybeInteger -> Integer
> maybeThree _ = 3

* Can you go back and simplify the `unMaybe` function using a
  wildcard?  Why or why not?

No, because there is no way to use the wildcard to meet the requirements of all the three conditions of unMaybe.
Or to simplify it. 

* Change the first line of the definition of `recordAge` to

    `recordAge (NameAndAge name age)          = age`

    Does this change the behavior of `recordAge`?  If so, how?  If
    not, in what circumstances would you prefer using one definition
    or the other?

    It doesn't change the behavior, but it gives a warning that name was defined but not used. 
    We would prefer using one definition in case we need it, otherwise we would prefer using wildcard. 

* What is the difference, if any, between the behavior of `recordAge`
  and `recordAge2`?  Describe what you think `case` does.

There is no difference in the behavior. case is used in defining multiple cases in one function definition. 

* Predict the values of `foo record1` and `foo record2`.  Were you
  right?

Predictions: 20, 23

![](../images/stop.gif) **When you reach this point, STOP and let Dr. Yorgey know.**

Recursive ADTs
--------------

**ROTATE ROLES**

> data Nat where
>   Z :: Nat
>   S :: Nat -> Nat
>   deriving Show
>
> three :: Nat
> three = S (S (S Z))
>
> natToInteger :: Nat -> Integer
> natToInteger Z     = 0
> natToInteger (S n) = 1 + natToInteger n
>
> natPlus :: Nat -> Nat -> Nat
> natPlus Z     n = n
> natPlus (S m) n = S (natPlus m n)
>
> data IntList where
>   Empty :: IntList
>   Cons  :: Integer -> IntList -> IntList
>   deriving Show
>
> intListLength :: IntList -> Integer
> intListLength Empty       = 0
> intListLength (Cons _ xs) = 1 + intListLength xs

* Give three different examples of values of type `Nat` (besides
  `three`).

> zero :: Nat
> zero = Z
> one :: Nat
> one = S Z
> two :: Nat
> two = S (S Z)


* Describe in English what values of type `Nat` look like.  Why do you
  think it is called `Nat`?

They are either of constructor S where they are passed another value of type
nat, or they are constructor Z where they are do not have an output. It sounds
like natural, so we think it might refer to natural numbers where the level of
recursion might refer to the corresponding natural number.

* What does `natToInteger` do?  How does it work?

It converts Nat to Integer by counting the number of layers of recursion.

* Try `natPlus` on some examples.  What does it do?  Can you explain
  how it works?

Recursively adds two Nats and returns a new Nat value.

* Give three different examples of values of type `IntList`.

> list1 :: IntList
> list1 = Cons 5 (Cons 6 Empty)

> list2 :: IntList
> list2 = Empty

> list3 :: IntList
> list3 = Cons 2 (Cons 3 (Cons 4 Empty))

* Describe in English what values of type `IntList` look like.

It looks like an integer, and an IntList, which contains the next integer,
and another IntList and so on until it reaches one of type Empty.

* Write a function `intListLengthNat :: IntList -> Nat` which works
  like `intListLength` but returns a `Nat` instead of an `Integer`.

    Note that it should be the case that any arbitrary value `list ::
    IntList` satisfies `natToInteger (intListLengthNat list) ==
    intListLength list`.

> intListLengthNat :: IntList -> Nat
> intListLengthNat Empty       = Z
> intListLengthNat (Cons _ nL) = S (intListLengthNat nL)

* Write a function `sumIntList :: IntList -> Integer` which adds up
  all the `Integer` values contained in an `IntList`.

> sumIntList :: IntList -> Integer
> sumIntList Empty       = 0
> sumIntList (Cons n nL) = n + sumIntList nL

* Write a function `incrIntList :: IntList -> IntList` which adds one
  to all the `Integer` values contained in an `IntList`.

> incrIntList :: IntList -> IntList
> incrIntList Empty = Empty
> incrIntList (Cons n nl) = Cons (n + 1) (incrIntList nl)

* Write a function `intListAppend :: IntList -> IntList -> IntList`
  which appends two `IntList`s together into one big `IntList`.

> intListAppend :: IntList -> IntList -> IntList
> intListAppend Empty Empty = Empty
> intListAppend (Cons n nl) Empty = Cons n nl
> intListAppend Empty (Cons n nl) = Cons n nl
> intListAppend (Cons n nl) (Cons m ml) = Cons n (Cons m (intListAppend nl ml))

* Create an algebraic data type called `ThreeTree`, such that values
  of type `ThreeTree` look like either
    + a `Leaf` containing an `Integer` value, or
    + a `Branch` with three children (of type `ThreeTree`).

    Don't forget to put `deriving Show` at the end of your definition
    so values of type `ThreeTree` can be displayed in GHCi.

> data ThreeTree where
>   Leaf   :: Integer -> ThreeTree
>   Branch :: ThreeTree -> ThreeTree -> ThreeTree -> ThreeTree
>   deriving Show

* Give three example values of type `ThreeTree`.

> leaf :: ThreeTree
> leaf = Leaf 6

> oneBranch :: ThreeTree
> oneBranch = Branch (Leaf 6) (Leaf 4) (Leaf 9)

> twoBranches :: ThreeTree
> twoBranches = Branch (Leaf 6) (Branch (Leaf 1) (Leaf 16) (Leaf 67)) (Leaf 0)

* Write a function `sumThreeTree :: ThreeTree -> Integer` which adds
  up all the `Integer` values contained in a `ThreeTree`.

> sumThreeTree :: ThreeTree -> Integer
> sumThreeTree (Leaf n)       = n
> sumThreeTree (Branch n m o) = sumThreeTree n + sumThreeTree m + sumThreeTree o

* Write a function `incrThreeTree :: ThreeTree -> ThreeTree` which
  adds one to all the `Integer` values contained in a `ThreeTree`.

> incrThreeTree :: ThreeTree -> ThreeTree
> incrThreeTree (Leaf n) = Leaf (n + 1)
> incrThreeTree (Branch n m o) = Branch (incrThreeTree n) (incrThreeTree m) (incrThreeTree o)

Feedback
--------

* How long would you estimate that you spent working on this module?

Three hours

* Were any parts particularly confusing or difficult?

Three Tree and the IntList were particularly confusing. It took some thinking
to figure out the recursion.

* Record here any questions, comments, or suggestions for improvement.

Make things easier please...