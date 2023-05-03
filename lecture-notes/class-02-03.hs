-- 02/03

-- Lists

data IntList where
    Empty :: IntList
    Cons  :: Int -> IntList -> IntList

-- e.g.
-- Empty
-- Cons 2 Empty
-- Cons 3(Cons 2 Empty)
-- Cons 6 (Cons 1 (Cons (-12) Empty))
-- ...

-- Normal built-in Haskell list:
-- - [] is empty list
-- - (:) is "cons", bulds a list from a single element and a list

-- e.g.
-- []
-- 2 : []
-- 3 : (2 : [])
-- 6 : (1 : ((-12) : []))
-- ...

-- parentheses not necessary
-- e.g. 6 : 1 : (-12) : []

-- and there is nicer syntax ("syntax sugar")
-- e.g. [6, 1, -12]

-- Fundamentally Haskell lists are singly-linked lists!

--  Very differnet from arrays or python lists, which are really glorified
--  arrays.

--  Operations like length, index, lookup, etc, all take O(n) time.

-- Haskel lists make great stacks.
-- Make great for loops... Haskell does not have for loops, but you can loop
-- element by element in the loop to emulate this.

-- Don't
--  - Use (!!) alot e.g. myList !! 24 (takes O(n))
--      not necessarily bad, but probably a sign that you need a different
--      data structure.
--  - Append a single item to the end of a list. e.g. myList ++ [x] (very slow)
--      better to attatch it on the front an reverse the list at the end.

--------------------------------------------------------------------------------

-- Polymorphism

-- < Greek: "poly" = many

-- polymorphism = code that can have many different types.

-- 1. "parametric polymorphism" = code that works exactly the same way for all
--  possible types

-- for example:

f :: a -> b -> a
f x y = x

-- Disco example:
-- f : a -> b -> a
-- f(x,y) = x