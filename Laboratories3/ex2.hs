-- FIRST PART OF THE EXERCISE
-- higher abstract functions:
-- sumWith, prodWith

-- -- sum' :: Num a => [a] -> a
-- -- sum' []     = 0
-- -- sum' (x:xs) = x + sum' xs

-- sumWith :: Num a => (a -> a) -> [a] -> a
-- sumWith _ []        = 0
-- sumWith f (x:xs)    = f x + sumWith f xs

-- sum'        = sumWith (\x -> x)
-- sumSqr'     = sumWith (\x -> x ^ 2)
-- sumCube'    = sumWith (\x -> x ^ 3)
-- sumAbs'     = sumWith (\x -> abs x)
-- -- ghci> sumWith (\x -> x ^ 5) [1..15]
-- listLength = sumWith (\x -> 1)

-- -- prod' :: Num a => [a] -> a
-- -- prod' []        = 1
-- -- prod' (x:xs)    = x * prod' xs

-- prodWith :: Num a => (a -> a) -> [a] -> a
-- prodWith _ []       = 1
-- prodWith f (x:xs)   = f x * prodWith f xs

-- prod'       = prodWith (\x -> x)
-- prodSqr'    = prodWith (\x -> x ^ 2)
-- prodCube'   = prodWith (\x -> x ^ 3)
-- prodAbs'    = prodWith (\x -> abs x)

-- SECOND PART OF THE EXERCISE
-- highest abstract function:
-- calcWith

-- Works fine for (+), (-), (*) and (/)
calcWith :: (Eq a, Num a) => (a -> a -> a) -> (a -> a) -> [a] -> a
calcWith op _ []
    | op 0 1 == (+) 1 0 = 0
    | op 0 1 == (*) 1 0 = 1
    | otherwise         = 0
calcWith op f (x:xs)    = op (calcWith op f xs) (f x)

sumWith     = calcWith (+)
prodWith    = calcWith (*)

sum'        = sumWith (\x -> x)
sumSqr'     = sumWith (\x -> x ^ 2)
sumCube'    = sumWith (\x -> x ^ 3)
sumAbs'     = sumWith (\x -> abs x)
-- ghci> sumWith (\x -> x ^ 5) [1..15]
listLength  = sumWith (\x -> 1)
sumSqrt'    = sumWith (\x -> sqrt x)

prod'       = prodWith (\x -> x)
prodSqr'    = prodWith (\x -> x ^ 2)
prodCube'   = prodWith (\x -> x ^ 3)
prodAbs'    = prodWith (\x -> abs x)
prodSqrt'   = prodWith (\x -> sqrt x)