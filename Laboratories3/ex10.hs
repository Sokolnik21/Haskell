isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc list = all (== True) $ zipWith (<=) list $ tail list

-- everySecond :: [t] -> [t]
-- everySecond []          = []
-- everySecond [x]         = [x]
-- everySecond (x:y:xs)    = x : everySecond xs

everySecond :: [t] -> [t]
everySecond list = map fst $ filter (odd . snd) $ zip list [1..]

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' _ _ []    = []
zip3' _ [] _    = []
zip3' [] _ _    = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' = foldl f ([],[],[])
    where
        f = \(x,x1,x2) (y,y1,y2) -> (x ++ [y], x1 ++ [y1], x2 ++ [y2])

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc list = all (== True) $ zipWith (>=) list $ tail list

isSorted :: Ord a => [a] -> Bool
isSorted list = isSortedAsc list || isSortedDesc list