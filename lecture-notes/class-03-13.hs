-- In the context of Either, (>>=) encapsulates the pattern wehre we have some
-- intermediate Either result, and we want to either quit with an error if it is
-- Left, or continue to do something with the value if it is Right.

-- (>>=) :: Either e a -> (a -> Either e b) -> Either e b

f :: Either String Int -> Either String Bool
f x = x >>= (\n -> if odd n then Left "odd" else Right (n > 20))

