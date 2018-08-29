import Control.Applicative

newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Applicative MyTriple where
  pure a = MyTriple (a, a, a)
  MyTriple (x1, y1, z1) <*> MyTriple (x2, y2, z2) =
    MyTriple (x1 x2, y1 y2, z1 z2)

instance Functor MyTriple where
  fmap f (MyTriple (x, y, z)) = MyTriple (f x, f y, f z)

f = MyTriple ((+1), (+2), (+3))
x = MyTriple (1, 2, 3)

res0 = f <*> x
res1 = pure (+2) <*> x
res2 = f <*> pure 2

data Tree2 a =  EmptyT2
              | Leaf a
              | Node (Tree2 a) a (Tree2 a) deriving Show

instance Applicative Tree2 where
  pure a = Leaf a
  EmptyT2 <*> _ = EmptyT2
  _ <*> EmptyT2 = EmptyT2
  Leaf a <*> Leaf b = Leaf (a b)
  Leaf a <*> Node t1 b t2 = Leaf (a b)
  Node t1 a t2 <*> Leaf b = Leaf (a b)
  Node t1 a t2 <*> Node t1sec b t2sec =
    Node (t1 <*> t1sec) (a b) (t2 <*> t2sec)

instance Functor Tree2 where
  fmap f EmptyT2    = EmptyT2
  fmap f (Leaf val) = Leaf (f val)
  fmap f (Node t1 val t2) = Node (fmap f t1) (f val) (fmap f t2)

t0 = EmptyT2
t1 = Leaf 1
t2 = Node (Node EmptyT2 1 (Node EmptyT2 2 EmptyT2)) 3 (Leaf 4)
t3 = Node (Leaf (+4)) (+3) (Node EmptyT2 (+1) (Node EmptyT2 (+2) EmptyT2))

-- order is important
tRes0 = t0 <*> t2
tRes1 = t3 <*> t1
tRes2 = t3 <*> t2

-- lifts
l1 = liftA (+1) (MkBox 1)
l2 = liftA2 (+) (MkBox 1) (MkBox 2)
l3 = liftA3 (\x y z -> ( x + y ) * z ) (MkBox 1) (MkBox 2) (MkBox 3)
