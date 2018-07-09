condition :: Int -> Int -> Int -> Bool
condition a b c
  | a ^ 2 + b ^ 2 == c ^ 2  = True
  | otherwise               = False

howManyRightTrianglesIn100 :: Int
howManyRightTrianglesIn100 =
  length [(a, b, c) |
    a <- [1..100], b <- [a..100], c <- [b..100],
    condition a b c]

-- non-optimal function
-- isPrime :: Integral t => t -> Bool
-- isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

howManyPrimesIn10000 :: Int
howManyPrimesIn10000 =
  length [i | i <- [2..10000], isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

-- optimal function
isPrime :: Int -> Bool
isPrime n =
  [i |
  i <- takeWhile (<= (floor $ sqrt $ fromIntegral n)) primes,
  n `mod` i == 0] == []

howManyPrimes :: Int -> Int
howManyPrimes n = length $ takeWhile (<= n) primes

allEqual :: Eq a => [a] -> Bool
allEqual xs = -- allEqual [1,1] = True, allEqual [1,2] = False
  all (== True) $ zipWith (\x y -> x == y) xs (tail xs)
