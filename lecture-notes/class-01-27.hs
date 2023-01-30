-- 01/27

-- Algabraic data types cont.

-- Given a set A of size a, and a set B of size B:
--      - How many elements are there in the union of A and B, assuming they
--          no elements in common? a + b.
--      - How many distinct pairs of elements are there where the first element
--          is from A and the second is from B? a * b

-- This kind of justifies thinking of union as "plus" and Cartesian product
-- (pairing) as "times".

-- Think of a type as a set of values.

-- If A and B are types, then A + B is the type of things which are either a
-- value of type A or a value of type B (together with a tag telling you which
-- it is). ("tagged union", "disjoint union")

-- If A and B are types, then A * B is the type of pairs of values, where the
-- first value has a type A and the second has a type B

-- Where do we see these in Haskell?

-- Product types:

p :: (Int, Bool) -- this is the "product type" Int * Bool
p = (3, True)

-- but also:

data IntBoolPair where
    P :: Int -> Bool -> IntBoolPair 
        -- a single constructor with two things inside is like a pair.

p2 :: IntBoolPair
p2 = P 3 True

-- Sum types:

data IntOrBool where
    I :: Int -> IntOrBool
    B :: Bool -> IntOrBool
        -- this is the "sum type" Int + Bool
        -- having two constructors is like an "or"

-- Any language with objects has product types:
--      an object gives us a way to store multiple values in one value, just
--      like (3, True) is a single value that contains multiple values.

-- Most 00 languages (Python, Java, ...) do NOT have some types

-- Rust has sum types.

-- Some languages have very simple sum types in th form of enumerations.

-- Claim: sum types are a kill feature for languages like Haskell, Rust.




data IntOrInt where
    X :: Int -> IntOrInt
    Y :: Int -> IntOrInt
        -- This is fine