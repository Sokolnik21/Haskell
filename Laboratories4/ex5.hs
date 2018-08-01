import Ex3

-- data MyInt = MkMyInt Int
newtype MyInt = MkMyInt Int
-- works the same

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

data MyDouble = MkMyDouble Double
  deriving (Show, Eq, Ord)
  -- only Num class is needed to implement

instance Eq a => Eq (BinTree a) where
  (==) EmptyBT EmptyBT  = True
  (==) EmptyBT _        = False
  (==) _ EmptyBT        = False
  (==) (NodeBT val1 lt1 rt1) (NodeBT val2 lt2 rt2)
    | val1 /= val2      = False
    | otherwise         = (==) lt1 lt2 && (==) rt1 rt2

data Cart3DVec a = Cart3DVec a a a
  -- deriving Ord

instance Eq a => Eq (Cart3DVec a) where
  (==) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) =
    x1 == x2 && y1 == y2 && z1 == z2

instance Ord a => Ord (Cart3DVec a) where
  (<=) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) =
    x1 <= x2 && y1 <= y2 && z1 <= z2

instance Num a => Num (Cart3DVec a) where
  (+) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) =
    Cart3DVec (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) =
    Cart3DVec (x1 - x2) (y1 - y2) (z1 - z2)
  (*) (Cart3DVec x1 y1 z1) (Cart3DVec x2 y2 z2) =
    Cart3DVec (x1 * x2) (y1 * y2) (z1 * z2)
  abs (Cart3DVec x y z) =
    Cart3DVec (abs x) (abs y) (abs z)
  signum (Cart3DVec x y z) =
    Cart3DVec (signum x) (signum y) (signum z)
  fromInteger int =
    Cart3DVec (fromIntegral int) (fromIntegral int) (fromIntegral int)

instance Show a => Show (Cart3DVec a) where
  show (Cart3DVec x y z) = "Cart3DVec " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z)

data Fraction a = Fraction {num::a, denom::a} -- num - numerator, denom - denominator

instance Show a => Show (Fraction a) where
  show a = "Fraction " ++ (show $ num a) ++ " " ++ (show $ denom a)

instance Eq a => Eq (Fraction a) where
  (==) a b = (==) (num a) (num b) && (==) (denom a) (denom b)

instance Ord a => Ord (Fraction a) where
  (<=) a b = (<=) (num a) (num b) && (<=) (denom a) (denom b)

instance (Num a, Floating a) => Num (Fraction a) where
  (+) a b = Fraction (num a + num b) (denom a + denom b)
  (-) a b = Fraction (num a - num b) (denom a - denom b)
  (*) a b = Fraction ( (num a * num b) + (denom a * denom b) )
    ( (num a * denom b) + (denom a * num b) )
  abs a = Fraction ( sqrt ( x + y ) ) 0
    where x = ( num a ) ** 2
          y = ( denom a ) ** 2
  signum a = signum $ abs a
  fromInteger int = Fraction (fromIntegral int) 0

newtype MyList a = MkMyList [a]
  deriving (Show, Eq)

instance Ord a => Ord (MyList a) where
  (<=) _ (MkMyList [])  = True
  (<=) (MkMyList []) _  = True
  (<=) (MkMyList (x:xs) ) (MkMyList (y:ys) )
    | x <= y            = (<=) (MkMyList xs)  (MkMyList ys)
    | otherwise         = False
