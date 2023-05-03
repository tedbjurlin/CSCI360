Module 03: Polymorphism and Lists
=================================

* Record your team members here: Ted Bjurlin, Dalton Casey, Taylor Aishman

In this module we will complete our initial exploration of the Haskell
programming language.  For today, choose whoever you wish to start as
the driver.

Maybe
-----

```{.haskell}
data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a
```

Note that the above definition of `Maybe` does not have bird tracks in
front of it since it is already defined in the standard library; I
just wanted to show you its definition.

> w :: Maybe Int
> w = Just 3
>
> x :: Maybe String
> x = Just "hello"
>
> y :: Maybe Char
> y = Nothing
>
> z :: Maybe (Maybe Int)
> z = Just Nothing
>
> safeDiv :: Integer -> Integer -> Maybe Integer
> safeDiv _ 0 = Nothing
> safeDiv x y = Just (x `div` y)
>
> showDiv :: Integer -> Integer -> String
> showDiv x y = case safeDiv x y of
>   Nothing -> "Division by zero"
>   Just z  -> "Result is " ++ show z

* Give two example values of type `Maybe Bool`.

> b1 :: Maybe Bool
> b1 = Just True

> b2 :: Maybe Bool
> b2 = Just False

* How many distinct values of type `Maybe (Maybe Bool)` are there?
  List all of them.

> mb1 :: Maybe (Maybe Bool)
> mb1 = Nothing

> mb2 :: Maybe (Maybe Bool)
> mb2 = Just Nothing

> mb3 :: Maybe (Maybe Bool)
> mb3 = Just (Just True)

> mb4 :: Maybe (Maybe Bool)
> mb4 = Just (Just False)

* How is `safeDiv` different from `div`?

It ensure the result is a real number. It does not allow division by zero.

* Try `showDiv` on some examples.  Describe in words what it does.

`showDiv` prints the result in a string. If it is a real number it rounds it to 
an integer and prints it in the format "Result is n" otherwise it prints
"Division by zero".

* What does the `a` in the definition of `Maybe` represent?

It represents a generic data type that can be replaced with another data type.

* What Java feature does the `a` remind you of?

Java generic type T.

* Write a function `plusMaybe :: Maybe Integer -> Maybe Integer ->
  Maybe Integer` which performs addition if both arguments are `Just`,
  and returns `Nothing` otherwise.

> plusMaybe :: Maybe Integer -> Maybe Integer -> Maybe Integer
> plusMaybe _ Nothing = Nothing
> plusMaybe Nothing _ = Nothing
> plusMaybe (Just n) (Just m) = Just (n + m)

![](../images/stop.gif) **(You know the drill)** You should be
prepared to share your version of `plusMaybe` with another group.

Lists
-----

**ROTATE ROLES**

> ints :: [Integer]
> ints = [3, 5, 92]
>
> noInts :: [Integer]
> noInts = []
>
> moreInts :: [Integer]
> moreInts = 7 : ints
>
> yetMoreInts :: [Integer]
> yetMoreInts = 4 : 2 : ints
>
> someInts :: [Integer]
> someInts = 8 : 42 : noInts
>
> ints2 :: [Integer]
> ints2 = 3 : 5 : 92 : []

* Evaluate `length ints` and `length noInts`.
length ints is 3
length noInts is 0

* Explain what `[]` means.

Its a list

* Evaluate `moreInts` and `length moreInts`.
moreInts adds a value to ints
length moreInts displays moreInts

* Do the same for `yetMoreInts`.
yetMoreInts adds more than one number to ints
length displays the amountcontained in yetMoreInts

* Now evaluate `ints`.  Has it changed?
No

* Write an expression `e` such that `length e` evaluates to `6`.

> e :: [Integer]
> e = [3, 5, 92, 6, 7, 9]


* Explain what the `(:)` operator does.
it adds an item

* What will `someInts` evaluate to?  How about `length someInts`?
  Write down your guesses *before* typing them into GHCi.
  list of 8 and 42 
  it would give a length of 2

* Now check your guesses.
we are correct

* Evaluate `ints2`.  What do you notice?
it only displays the integers/ the list as it should
it gives a warning

![](../images/stop.gif)

Strings
-------

**ROTATE ROLES**

> greeting :: String
> greeting = "Hello"
>
> greeting2 :: String
> greeting2 = ['H','e','l','l','o']
>
> greeting3 :: [Char]
> greeting3 = ['H','e','l','l','o']
>
> everyone :: String
> everyone = "world"

* Evaluate `greeting`, `greeting2`, and `greeting3`.  What
  differences do you notice?  What can you conclude? (Hint: try typing
  `:info String` at the GHCi prompt.)
  greeting3 is outputed/typed as a list of characters while greeting and 
  greeting2 are outputed/typed as a full string

* Try evaluating `greeting : everyone`.  What happens?
<interactive>:38:12: error:
    • Couldn't match type ‘Char’ with ‘[Char]’
      Expected: [String]
        Actual: String
    • In the second argument of ‘(:)’, namely ‘everyone’
      In the expression: greeting : everyone
      In an equation for ‘it’: it = greeting : everyone

  We got this. : expects a single item but instead gets a full list, this 
  causes the error. 

* Now try evaluating `greeting ++ everyone`.  What happens?
  Outputs: "Helloworld"
  This actually combined the two lists without error.

* Explain the difference between `(:)` and `(++)`.
  : combines a single item and in a list, while ++ joins two whole lists.

* What are the types of `(:)` and `(++)`?  Do they match your
  explanation above?
  ghci> :info :
  type [] :: * -> *
  data [] a = ... | a : [a]

  ghci> :info ++
  (++) :: [a] -> [a] -> [a] 	-- Defined in ‘GHC.Base’

  What this shows is : combining a single item into a list while ++ combines two lists
  like we said. 


* Explain the difference between `'a'` and `"a"`.
  ghci> :type 'a'
  'a' :: Char
  ghci> :type "a"
  "a" :: String
  Single quotes indicate char, double indicate string. 


* Write an expression using `greeting` and `everyone` which evaluates
  to `"Hello, world!"`.

> helloworld :: String
> helloworld = greeting ++ ", " ++ everyone ++ "!"

![](../images/stop.gif)

List pattern matching
---------------------

**ROTATE ROLES**

> listLength []     = 0 :: Integer
> listLength (_:xs) = 1 + listLength xs
>
> startsWith :: String -> String -> Bool
> startsWith []     _      = True
> startsWith (_:_)  []     = False
> startsWith (x:xs) (y:ys) = startsWith xs ys && x == y

* What is the type of `listLength`? (Feel free to ask GHCi.)

listLength :: [a] -> integer

* The type of `listLength` probably has a lowercase letter in it, like
  `t` or `a`.  Explain what the type of `listLength` means.

It takes in a generic type and gives the length

* Evaluate `startsWith "cat" "catastrophe"`.  What happens?

*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at 03-polymorphism-lists.lhs:244:30 in main:Main
ghci> 


* Complete the definition of `startsWith` by replacing `undefined`
  with appropriate expressions.  `startsWith` should test whether the
  second argument has the first argument as a prefix.  For example:

    ```
    startsWith "cat" "catastrophe" -> True
    startsWith "car" "catastrophe" -> False
    startsWith "ban" "banana"      -> True
    startsWith ""    "dog"         -> True
    startsWith "at"  "catastrophe" -> False
    startsWith "dog" ""            -> False
    ```

* Write a function `contains :: String -> String -> Bool`, which tests
  whether the second argument contains the first argument as a
  (contiguous) substring.  For example,


> contains :: String -> String -> Bool
> contains _ [] = False
> contains x (y:ys) = contains x ys || startsWith x (y:ys)




    ```
    contains "cat" "catastrophe" -> True
    contains "cat" "concatenate" -> True
    contains "cat" "create"      -> False
    contains "fly" "old lady"    -> False
    ```

    Hint: use `startsWith`.

* Write a function `listReverse :: [a] -> [a]` which reverses a list.
  For example,

> listReverse :: [a] -> [a] 
> listReverse [] = []
> listReverse (x:xs) = listReverse xs ++ [x]



    ```
    listReverse []      -> []
    listReverse [1,2,3] -> [3,2,1]
    listReverse "Hello" -> "olleH"
    ```

    **DO NOT** look at any existing implementation of reverse.

