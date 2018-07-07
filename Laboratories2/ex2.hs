fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^) -- fiveToPower_ 3 = 125

-- _ToPower5 :: Num a => a -> a
-- _ToPower5 = (^ 5) -- _ToPower5 2 = 32

-- subtrNFrom5 :: Num a => a -> a
-- subtrNFrom5 = (5 -) -- subtrNFrom5 3 = 2

-- via anonymous function
_ToPower5 :: Num a => a -> a
_ToPower5 = \x -> x ^ 5 -- _ToPower5 2 = 32

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = \x -> 5 - x -- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a
subtr5From_ = \x -> x - 5 -- subtr5From_ 6 = 1

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = \x y -> f y x

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f = \x y z -> f z y x
