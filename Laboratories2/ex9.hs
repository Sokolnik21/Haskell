-- qSort :: Ord a => [a] -> [a]
-- qSort []    = []
-- qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
--     where
--         leftPart xs     = [y | y <- xs, y <= x]
--         rightPart xs    = [y | y <- xs, y > x]

qSort :: Ord a => [a] -> [a]
qSort []    = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart xs     = filter (<= x) xs
        rightPart xs    = filter (> x)  xs

-- MergeSort - start
mSort :: Ord a => [a] -> [a]
mSort []    = []
mSort z =
    let z1 = prepare z
    in let z2 = mergePieces z1
    in let z3 = backToList z2
    in z3
        where backToList [x] = x

prepare :: Ord a => [a] -> [[a]]
prepare []  = []
prepare (x:xs) = [[x]] ++ (prepare xs)

mergePieces :: Ord a => [[a]] -> [[a]]
mergePieces base =
    if (length base) == 1 then base
    else mergePieces (iterationMerge base)

iterationMerge :: Ord a => [[a]] -> [[a]]
iterationMerge (x:xs)
    | length (x:xs) == 0    = [] 
    | length (x:xs) == 1    = [x]
    | length (x:xs) == 2    = [merge x (head xs)]
    | length (x:xs) == 3    = [merge x (head xs)] ++ [xs !! 1]
    | otherwise             = [merge x (head xs)] ++ iterationMerge (drop 1 xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] z  = z
merge z []  = z
merge (x:xs) (y:ys)
    | x <= y    = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys
-- MergeSort - end

-- InsertionSort - start
iSort :: Ord a => [a] -> [a]
iSort []    = []
iSort base  = loop [] base
    where
        loop result []  = result
        loop result (x:xs) = loop (insert result x) xs

insert :: Ord a => [a] -> a -> [a]
insert []   elem = [elem]
insert list elem =
    if (last list) < elem then list ++ [elem]
    else insert (init list) elem ++ [last list]
-- InsertionSort - end

concat' :: [[a]] -> [a]
concat' []      = []
concat' (x:xs)  = [result | result <- x] ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' base = loop [] base
    where   loop res []   = res
            loop res (x:xs) = loop (res ++ x) xs

isSorted :: [Int] -> Bool -- isSorted [1,2,2,3] = True
isSorted []     = True
isSorted [x]    = True
isSorted (x:xs) =
    if x <= head xs then isSorted xs
    else False

reverse' :: [a] -> [a] -- reverse [1,2,3] = [3,2,1]
reverse' []     = []
reverse' [x]    = [x]
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)] -- zip' [1,2] [3,4] = [(1,3), (2,4)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y:ys)  = [(x, y)] ++ zip' xs ys

unzip' :: [(a, b)] -> ([a],[b]) -- unzip [(1,2), (3,4)] = ([1,3],[2,4])
unzip' []       = ([],[])
unzip' (x:xs)   = loop ([], []) (x:xs)
    where
        loop z []           = z
        loop (i, j) (x:xs)  = loop (i ++ [fst x], j ++ [snd x]) xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ []                = []
zip3' _ [] _                = []
zip3' [] _ _                = []
zip3' (x:xs) (y:ys) (z:zs)  = [(x, y, z)] ++ zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool -- subList [1,2] [3,1,2,4] = True
subList sample list = loop sample sample list list
    where
        loop [] _ _ _ = True
        loop _ _ [] _ = False
        loop (x:xs) baseSample (y:ys) baseList = 
            if (x == y) then loop xs baseSample ys baseList
            else loop baseSample baseSample (tail baseList) (tail baseList)