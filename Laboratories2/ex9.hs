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