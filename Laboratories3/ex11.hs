concat' :: [[a]] -> [a]
concat' []      = []
concat' (x:xs)  = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' []     = []
concat'' (x:xs) = [z | z <- x] ++ concat'' xs

concat''' :: [[a]] -> [a]
concat''' = foldr (\x acc -> x ++ acc) []

answer1 = concat $ map (\x -> [x * 2]) [1..5]
answer2 = concatMap (\x -> [x * 2]) [1..5]
answer3 = concatMap (\x -> x ++ ['!']) ["Ready", "Steady", "Go"]