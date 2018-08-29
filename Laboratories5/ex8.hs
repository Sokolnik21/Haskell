data Tree2 a = EmptyT2 | Leaf a | NodeT2 (Tree2 a) a (Tree2 a) deriving Show

t0 = EmptyT2
t1 = Leaf 1
t2 = NodeT2 (NodeT2 EmptyT2 1 (NodeT2 EmptyT2 2 EmptyT2)) 3 (Leaf 4)
t3 = NodeT2 (Leaf (+4)) (+3) (NodeT2 EmptyT2 (+1) (NodeT2 EmptyT2 (+2) EmptyT2))

instance Foldable Tree2 where
  foldMap f EmptyT2        = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (NodeT2 l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

res0 = foldMap show t2

instance Functor Tree2 where
  fmap f EmptyT2        = EmptyT2
  fmap f (Leaf x)       = Leaf $ f x
  fmap f (NodeT2 l x r) = NodeT2 (fmap f l)
                                 (f x)
                                 (fmap f r)

res1 = fmap (+1) t2

instance Traversable Tree2 where
  traverse f EmptyT2        = pure EmptyT2
  traverse f (Leaf x)       = Leaf <$> f x
  traverse f (NodeT2 l x r) = NodeT2 <$> traverse f l
                                     <*> f x
                                     <*> traverse f r

res2 = traverse id t3 1
res3 = traverse (\x -> const 11) t3 1
