not' :: Bool -> Bool
not' True = False
not' False = True

-- Pattern mathcing works similar to switch ... cases
-- When a pattern is matched then program uses function that is connected with that Pattern
-- Without checking patterns below
-- [Divagation] in C++ pattern matching can be done with templates, however
-- in C++ along with templates comes specialization that has higher priority
-- than templates
-- Haskell is simpler; he works like switch
-- So, in the example below the answer'll be always [False]
-- To change it You should move "Love" case over _ (default) case
isItTheAnswer :: String -> Bool
isItTheAnswer _       = False
isItTheAnswer "Love"  = True

or' :: (Bool, Bool) -> Bool
or' (True, _) = True
or' (_, True) = True
or' (_, _)    = False

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (_, _)       = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True)  = False
nand' (_, _)        = True

xor' :: (Bool, Bool) -> Bool
xor' (True, True)   = False
xor' (False, False) = False
xor' (_, _) = True

-- Boolean Function Optimization:
-- 1. Algebraic Manipulation
-- 2. Karnaugh map
orKM :: (Bool, Bool) -> Bool
or' (False, False)  = False
or' (_, _)          = True
