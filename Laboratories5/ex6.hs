{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)
instance Functor BinTree where
  fmap f EmptyBT = EmptyBT
  fmap f (NodeBT a lt rt) =
    NodeBT (f a) (fmap f lt) (fmap f rt)

bt = (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 2 EmptyBT EmptyBT))

newtype Pair b a = Pair { getPair :: (a,b) } deriving Show -- fmap should change the first element
instance Functor (Pair a) where
    fmap f (Pair (x, y)) = Pair (f x, y)

data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show
instance Functor Tree2 where
  fmap f EmptyT2 = EmptyT2
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt a rt) =
    Node (fmap f lt) (f a) (fmap f rt)

t2 = (Node (Node (Leaf 4) 6 (Leaf 5)) 1 (Node (Leaf 2) 3 EmptyT2))

data GTree a = GLeaf a | GNode [GTree a] deriving Show
instance Functor GTree where
  fmap f (GLeaf a) = GLeaf (f a)
  fmap f (GNode []) = GNode []
  fmap f (GNode (x:xs)) = connectGTree (fmap f x) (fmap f (GNode xs))

connectGTree :: GTree a -> GTree a -> GTree a
connectGTree (GLeaf l1) (GLeaf l2) = GNode ([GLeaf l1] ++ [GLeaf l2])
connectGTree (GLeaf l1) (GNode g2) = GNode ([GLeaf l1] ++ g2)
connectGTree (GNode g1) (GLeaf l2) = GNode ([GNode g1] ++ [GLeaf l2])
connectGTree (GNode g1) (GNode g2) = GNode ([GNode g1] ++ g2)

gt = (GNode [GLeaf 1, GLeaf 2, GNode [GLeaf 3, GNode []]])

-- -- already defined
-- instance Functor ((->) r) where
--   fmap = (.)
