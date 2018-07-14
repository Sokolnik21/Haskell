isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n - 1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n    | n < 0     = False
            | n == 0    = True
            | otherwise = isOdd (n - 1)

-- I tried to use functions from above with large numbers (to cause overflow)
-- but the calculations were so long that I had to abort it

-- The time complexity is: O(n)
-- The memory complexity is also: O(n)

ackerFun m n
    | m == 0    = n + 1
    | n == 0    = ackerFun (m - 1) 1
    | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))

-- Max (m, n) values that ackerFun can calculate (in tolerable time):
-- (4, 0) - used 0 bytes
-- (3, 9) - used 5,027,615,896 bytes
-- (2, 4000) - used 14,416,373,040 bytes
-- (1, 4000000) - used 3,550,379,360 bytes
-- (0, n) - not so special

-- ackerFun (3, y)
-- 3 3 : 0.01s
-- 3 4 : 0.02s
-- 3 5 : 0.05s
-- 3 6 : 0.17s
-- 3 7 : 0.67s
-- 3 8 : 2.81s
-- 3 9 : 10.87s

-- ackerfun (x, 3)
-- 0 3 : 0.01s
-- 1 3 : 0.01s
-- 2 3 : 0.01s
-- 3 3 : 0.01s
-- 4 3 : too long to calcutate

-- Time complexity of this function is really huge
-- This function is usually used to test compilers 
