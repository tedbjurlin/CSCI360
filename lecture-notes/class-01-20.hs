-- 01/20

-- 3 things about haskell
-- - strong static typing
-- - pure
-- - functional

-- Strong static typing

-- types can be declared, but also inferred

x :: Int
x = 5

y = 6 -- y :: Integer

-- Types are checked before the program runs
--     = "static" types
-- "dynamic" types = types checked at runtime
--     e.g. Python

-- Pure

-- 1. variables are immutable
--     assigning a variabll is basically saying that it is a new name for the value.

-- How do we do anything if we cant mutate variables

-- We use recursion. When a function calls itself it could have a new value for the input

-- 2. referential transparency
--     "we can always replave equals by equals"

-- def g():
--     return f() + f()

-- def g2():
--     x = f()
--     return x + x

-- Are g and g2 the same? If we had referential transparency, yes they would be the same.
-- In python we do not have referential transparency. If f returned a random number
-- it would not work.

-- In Haskell, every function *always* returns the same output given the same input.
-- In Haskell, functions cannot do or effect anything aside from the output.



