-- Introduction
sqr x = x ^ 2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^ 3)
    4 -> \x -> x ^ 4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x ^ 5

-- Exercise 3
expApproxUpTo :: Int -> Double -> Double
expApproxUpTo 0 = \_ -> 1
expApproxUpTo n = \x -> (x ^ n) / fromIntegral (silnia n) + expApproxUpTo (n - 1) x
    where
        silnia 0    = 1
        silnia x    = x * silnia (x - 1)