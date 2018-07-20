sumWith g []        = 0
sumWith g (x:xs)    = g x + sumWith g xs

prodWith g []       = 1
prodWith g (x:xs)   = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
    where
        go acc g []     = acc
        go acc g (x:xs) = go (acc + g x) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
    where
        go acc g []     = acc
        go acc g (x:xs) = go (acc * g x) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []       = z
foldr' f z (x:xs)   = f x $ foldr' f z xs

sumWith'' g     = foldr' (\x acc -> g x + acc) 0
prodWith'' g    = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []       = z
foldl' f z (x:xs)   = f (foldl' f z xs) x

sumWith''' g     = foldl' (\acc x -> g x + acc) 0
prodWith''' g    = foldl' (\acc x -> g x * acc) 1

map' f  = foldr (\x acc -> f x : acc) []
map'' f = \list -> reverse $ foldl (\acc x -> f x : acc) [] list

filter' p   = foldr (\x acc ->
    if p x == True then x : acc
    else acc) []
filter'' p  = \list -> reverse $ foldl (\acc x ->
    if p x == True then x : acc
    else acc) [] list

foldl'' f s list = foldr (flip f) s $ reverse list
foldr'' f s list = foldl (flip f) s $ reverse list