-- product type example (one constructor)
type Xint = Int
type Yint = Int

data CartInt2DVec = MkCartInt2DVec Xint Yint -- konwencja: prefix 'Mk' dla konstruktora
xCoord :: CartInt2DVec -> Xint
xCoord (MkCartInt2DVec x _) = x
yCoord :: CartInt2DVec -> Yint
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a
xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x
yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}
-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal
-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a
xCoord3D (Cart3DVec x _ _) = x
yCoord3D (Cart3DVec _ y _) = y
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x3d::a, y3d::a, z3d::a}

data Polar2DVec a = Polar2DVec a a
polarToCart (Polar2DVec r phi) = MkCart2DVec' (r * cos phi) (r * sin phi)
data Polar2DVec' a = Polar2DVec' {r::a, phi::a}
polarToCart' polar = MkCart2DVec' (radius * cos angle) (radius * sin angle)
  where
    radius = r polar
    angle = phi polar

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area s = case s of
  (Circle r)      -> pi * r ** 2
  (Rectangle a b) -> a * b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error "empty Tree"
rootValue (Node a _ _) = a

data TrafficLights =
  RedTL |
  OrangeTL |
  GreenTL

actionFor :: TrafficLights -> String
actionFor RedTL     = "Stay"
actionFor OrangeTL  = "Alertness"
actionFor GreenTL   = "Drive"

data DriverBehavior =
  Stay |
  Alertness |
  Drive

actionFor' :: TrafficLights -> DriverBehavior
actionFor' RedTL    = Stay
actionFor' OrangeTL = Alertness
actionFor' GreenTL  = Drive
