-- onlyEven [] = []
-- onlyEven (x:xs)
--     | x `mod` 2 == 0    = x : onlyEven xs
--     | otherwise         = onlyEven xs

-- onlyOdd []  = []
-- onlyOdd (x:xs)
--     | x `mod` 2 == 1    = x : onlyOdd xs
--     | otherwise         = onlyOdd xs

-- onlyUpper []    = []
-- onlyUpper (x:xs)
--     | isCapitalized x   = x : onlyUpper xs
--     | otherwise         = onlyUpper xs
--     where
--         isCapitalized c
--             | fromEnum c >= fromEnum 'A' && fromEnum c <= fromEnum 'Z'  = True
--             | otherwise = False

-- Higher abstract
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []    = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

onlyEven    = filter' (\x -> x `mod` 2 == 0)
onlyOdd     = filter' (\x -> x `mod` 2 == 1)
onlyUpper   = filter' (\x -> (isCapitalized) x)
    where
        isCapitalized :: Char -> Bool
        isCapitalized c = (fromEnum c >= fromEnum 'A') && (fromEnum c <= fromEnum 'Z')