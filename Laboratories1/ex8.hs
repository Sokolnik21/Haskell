-- not' :: Bool -> Bool
-- not' b = case b of
--   True  -> False
--   False -> True

-- absInt :: Int -> Int -- with this function works only with integers
absInt n =
  case (n >= 0) of
    True  -> n
    _     -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s =
  case s of
    "answer"  -> True
    _         -> False

not' :: Bool -> Bool
not' a =
  case a of
    True  -> False
    False -> True

or' :: (Bool, Bool) -> Bool
or' (a, b) =
  case (a, b) of
    (False, False)  -> False
    (_, _)          -> True

and' :: (Bool, Bool) -> Bool
and' (a, b) =
  case (a, b) of
    (True, True)  -> True
    (_, _)        -> False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) =
  case (a, b) of
    (True, True)  -> False
    (_, _)        -> True

-- finally able to create xor only with one comparision
xor' :: (Bool, Bool) -> Bool
xor' (a, b) =
  case a /= b of
    True  -> True
    False -> False
