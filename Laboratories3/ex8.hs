-- Part 1
-- doubleElems []      = []
-- doubleElems (x:xs)  = 2 * x : doubleElems xs

-- sqrElems []     = []
-- sqrElems (x:xs) = x ^ 2 : sqrElems xs

-- lowerCase []        = []
-- lowerCase (x:xs)    = lower x : lowerCase xs
--     where
--         lower :: Char -> Char
--         lower c
--             | (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')  = toEnum (fromEnum c + 32)
--             | otherwise = c

-- Part 2
-- Higher abstract function
map' :: (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x:xs)   = f x : map' f xs

doubleElems = map' (2 *)
sqrElems    = map' (^ 2)
lowerCase   = map' lower
    where
        lower :: Char -> Char
        lower c
            | (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')  = toEnum (fromEnum c + 32)
            | otherwise = c

-- Part 3
-- List comprehensions
-- doubleElems list    = [2 * x | x <- list]
-- sqrElems list       = [x ^ 2 | x <- list]
-- lowerCase list      = [lower x | x <- list]
--     where
--         lower :: Char -> Char
--         lower c
--             | (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')  = toEnum (fromEnum c + 32)
--             | otherwise = c

-- Observation: list comprehensions are slower than recursive implementation