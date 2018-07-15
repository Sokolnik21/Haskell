fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstModSnd :: Integral a => [a] -> Bool
fstModSnd (x : y : _) | ((y `mod` x) == 0)  = True
fstModSnd _                                 = False

fstModTrd :: Integral a => [a] -> Bool
fstModTrd (x : y : z : _) | ((z `mod` x) == 0)  = True
fstModTrd _                                     = False