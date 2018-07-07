myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

-- operator "->" makes operations from right to left
add2C :: Num a => a -> (a -> a)
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (a, b, c) = a + b + c

add3C :: Num a => a -> a -> a -> a
add3C a b c = a + b + c

-- tricky problem
curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(x, y) -> f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = \x y z -> f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(x, y, z) -> f x y z
