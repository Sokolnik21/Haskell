{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
  if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)
-- fib.O(n) = 1.6 ^ n

fib2 :: Int -> Int
fib2 n =
  let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
  in fibs !! n
-- fib2.O(n) = n

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs
-- overflow between 10000000 and 12000000

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' []      = 1
prod' (x:xs)  = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' []      = 0
length' (x:xs)  = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' []  = False
or' (x:xs)
  | x == False  = or' xs
  | otherwise   = True

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' []  = True
and' (x:xs)
  | x == True = and' xs
  | otherwise = False

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' _ []  = False
elem' e (x:xs)
  | e == x    = True
  | otherwise = elem' e xs

doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]
doubleAll []  = []
doubleAll (x:xs) = (x * 2) : doubleAll xs

squareAll :: Num t => [t] -> [t] -- double squareAll [2,3] = [4,9]
squareAll []  = []
squareAll (x:xs) = (x ^ 2) : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs)
  | x `mod` 2 == 0  = x : selectEven xs
  | otherwise       = selectEven xs

arithmethicMean :: Floating t => [t] -> t
arithmethicMean x = (sum x) / (fromIntegral (length x))

geometricMean :: Floating t => [t] -> t
geometricMean x = sqrt (product x)

meansArithGeo :: Floating t => [t] -> (t, t)
meansArithGeo x = (arithmethicMean x, geometricMean x)

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs
-- overflow between 12000000 and 13000000        

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
  where loop acc []     = acc
        loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
  where loop acc []     = acc
        loop acc (x:xs) = loop (acc + 1) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
    where loop !acc []     = acc
          loop !acc (x:xs) = loop (x + acc) xs
-- didn't noticed overflow (even at 100000000-size array [almost 10x larger])