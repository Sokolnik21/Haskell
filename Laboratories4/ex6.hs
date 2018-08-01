class Mappable t where
  fMap :: (a -> b) -> t a -> t b
  -- fMap :: (a -> a) -> t a -> t a

data Vec3D a = Vec3D {x::a, y::a, z::a}
  deriving Show

instance Mappable Vec3D where
  fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

newtype Pair a = Pair (a, a)
  deriving Show

instance Mappable Pair where
  fMap f (Pair (x, y)) = Pair (f x, f y)

data BinTree a =  EmptyBT |
                  NodeBT a (BinTree a) (BinTree a)
                  deriving Show

instance Mappable BinTree where
  fMap f EmptyBT            = EmptyBT
  fMap f (NodeBT val lt rt) = NodeBT (f val) (fMap f lt) (fMap f rt)

instance Mappable Maybe where
  fMap f (Just val) = Just (f val)
  fMap _ Nothing    = Nothing

instance Mappable (Either a) where
  fMap f (Right a)  = Right (f a)
  fMap f (Left a)   = Left a
-- [IMPORTANT] Left value is used usually as an [error]
-- that's why in Left case there is no evaluation

instance Mappable ((->) a) where
  fMap f g = \x -> f (g x)

class VectorLike t where
  (|==|) :: Eq a => t a -> t a -> Bool
  (|+|), (|-|) :: (Num a) => t a -> t a -> t a
  (|*|) :: (Num a) => t a -> t a -> t a
  (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
  vectLength :: Floating a => t a -> a
  unitVectOf :: Floating a => t a -> t a

class CrossProductable t where
  crossProduct :: (Num a, Eq a) => t a -> t a -> a
instance CrossProductable Vec2D where
  crossProduct (Vec2D x1 y1) (Vec2D x2 y2) = x1 * y2 - x2 * y1
instance CrossProductable Vec3D where
  crossProduct (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) =
    (y1 * z2 - z1 * y2) - (x1 * z2 - z1 * x2) - (x1 * y2 - y1 * x2)

instance VectorLike Vec3D where
  (|==|) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2
  (|+|) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
  (|-|) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
  (|*|) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1 * x2) (y1 * y2) (z1 * z2)
  (||?) v1 v2 = crossProduct v1 v1 == 0
  (|-?) v1 v2 = crossProduct v1 v1 == 1
  vectLength (Vec3D x y z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
  unitVectOf (Vec3D x y z) = Vec3D (x / c) (y / c) (z / c)
    where c = vectLength (Vec3D x y z)

data Vec2D a = Vec2D {x2::a, y2::a}

instance VectorLike Vec2D where
  (|==|) (Vec2D x1 y1) (Vec2D x2 y2) = x1 == x2 && y1 == y2
  (|+|) (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
  (|-|) (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)
  (|*|) (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 * x2) (y1 * y2)
  (||?) v1 v2 = crossProduct v1 v1 == 0
  (|-?) v1 v2 = crossProduct v1 v1 == 1
  vectLength (Vec2D x y) = sqrt (x ^ 2 + y ^ 2)
  unitVectOf (Vec2D x y) = Vec2D (x / c) (y / c)
    where c = vectLength (Vec2D x y)
