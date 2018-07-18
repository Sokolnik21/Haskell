import Data.List

-- ghci> :i (.)
-- "infixr 9" means that (.) is the lowest proritet

-- "point-free" == Tacit programming
-- ~ functions without arguments (made of other functions)
sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt _ _ []     = True
are2FunsEqAt f g (x:xs) -- are2FunsEqAt (+2) (\x -> x + 2) [1..1000] = True
    | f x == g x        = are2FunsEqAt f g xs
    | f x /= g x        = False

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = \x -> f (g x)

composeFunList :: [a -> a] -> (a -> a)
composeFunList = foldl1 (>.>)