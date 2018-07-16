-- Anonymous functions
foo1 :: Num a => a -> a
foo1 = \x -> x - 2

foo2 :: Double -> Double -> Double
foo2 = \x y -> sqrt (x ^ 2 + y ^ 2)

foo3 :: Double -> Double -> Double -> Double
foo3 = \x y z -> sqrt (x ^ 2 + y ^ 2 + z ^ 2)

foo4 = \x -> 2 * x
foo5 = \x -> x * 2
foo6 = \x -> 2 ^ x
foo7 = \x -> x ^ 2
foo8 = \x -> 2 / x
foo9 = \x -> x / 3
foo10 = \x -> 4 - x

-- Babylonian method
sqrt' = \x -> sqrt'Iteration x 1 1e-8
    where
        sqrt'Iteration base result epsilon =
            if abs((result ^ 2) - base) < epsilon then result
            else sqrt'Iteration base ((1/2) * (result + base/result)) epsilon
abs' = \x -> if x < 0 then -x else x
-- Taylor series expansion of logarithmic function
log' = \x -> log'Iteration x 0 1 1e-8
    where
        log'Iteration base result n epsilon =
            if (element base n) < epsilon then result
            else log'Iteration base (result + (element base n)) (n + 1) epsilon
                where
                    element :: Double -> Double -> Double
                    element base n = 2 * (1 / ((2 * n) - 1)) * ((base - 1) / (base + 1)) ^ floor ((2 * n) - 1)
id' = \x -> x
const' = \x _ -> x

-- f7 x = if x `mod` 2 == 0 then True else False
f7 = \x -> if x `mod` 2 == 0 then True else False
-- f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8 = \x ->
    let y = sqrt x
    in 2 * y ^ 3 * (y + 1)
-- f9 1 = 3
-- f9 _ = 0
f9 = \x ->
    if x == 1 then 3
    else 0

-- Interesting fact:
-- It is not possible to code such functions:
-- f9 = \1 -> 3
-- f9 = \_ -> 0
-- Why?
-- Because then both functions would have same exactly the same
-- arguments list as an input. So, the compiler couldn't recognise
-- which one use.