module Ex1 where

(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- -- (>^$>) = extract (^) and apply ($)
-- (>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- ma >^$> f = (extractMaybe ma) >$> f
-- infixl 1 >^$>

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _  = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (x + 1) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x /= 0 then Just (10 * x) else Nothing

-- Kleisli composition
-- (>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> g (extractMaybe (f x))

-- EXERCISES
joinMaybe :: Maybe (Maybe a) -> (Maybe a)
joinMaybe Nothing   = Nothing
joinMaybe (Just x)  = x

(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
-- f >.>> g = \x -> (f x) >^$> g
f >.>> g = \x -> joinMaybe $ fmap g $ joinMaybe $ fmap f (Just x)

-- Annotation:
-- I think it's impossible to do such thing as function that takes IO type and return "clear" type
-- Why?
-- Because IO actions are dirty and once Haskell encounters some it should keep them away from the rest
-- extractIO :: IO String -> String -- not possible

insertTuple :: a -> (a, String)
insertTuple a = (a, "Tuple")

extractTuple :: (a, String) -> a
extractTuple a = fst a

(>^^$>) :: (a, String) -> (a -> (b, String)) -> (b, String)
(a, s) >^^$> f = f a
infixl 1 >^^$>

(>^.>>) :: (a -> (b, String)) -> (b -> (c, String)) -> (a -> (c, String))
f >^.>> g = \x -> (f x) >^^$> g

fT1 :: (Ord a, Num a) => a -> (a, String)
fT1 x = if x > 0 then insertTuple (x + 1) else insertTuple x

fT2 :: (Eq a, Num a) => a -> (a, String)
fT2 x = if x /= 0 then insertTuple (x * 10) else insertTuple x
