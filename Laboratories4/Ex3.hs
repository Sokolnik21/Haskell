module Ex3 where

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

-- data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
--               Add (Expr a) (Expr a)
--
-- eval :: Num a => Expr a -> a
-- eval (Lit n) = n
-- eval (Add e1 e2) = eval e1 + eval e2
--
-- show' :: Show a => Expr a -> String
-- show' (Lit n) = show n
-- show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

-- For testing
-- let x = (NodeBT 1 (NodeBT 2 (NodeBT 4 EmptyBT EmptyBT) (NodeBT 5 EmptyBT EmptyBT)) (NodeBT 3 EmptyBT EmptyBT))

-- 1
depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBTPreorder :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT val lt rt) = [val] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT val lt rt) = flattenBTInorder lt ++ [val] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT val lt rt) = flattenBTPostorder lt ++ flattenBTPostorder rt ++ [val]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT val lt rt) = (NodeBT (f val) (mapBT f lt) (mapBT f rt))

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT val lt rt)
  | a < val   = NodeBT val (insert a lt) rt
  | a > val   = NodeBT val lt (insert a rt)
  | otherwise = NodeBT a (NodeBT val lt EmptyBT) rt

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST = foldr insert EmptyBT

-- 2
occurs :: Eq a => a -> BinTree a -> Int -- liczba wystąpień elementu w drzewie binarnym
occurs _ EmptyBT  = 0
occurs a (NodeBT val lt rt)
  | val == a  = 1 + occurs a lt + occurs a rt
  -- | otherwise = 0 + occurs a lt + occurs a rt

elemOf :: Eq a => a -> BinTree a -> Bool -- sprawdzenie, czy element znajduje się w drzewie
elemOf _ EmptyBT  = False
elemOf a (NodeBT val lt rt)
  | val == a  = True
  | otherwise = elemOf a lt || elemOf a rt

reflect :: BinTree a -> BinTree a -- 'odbicie lustrzane' drzewa binarnego
reflect EmptyBT = EmptyBT
reflect (NodeBT val lt rt) = (NodeBT val (reflect rt) (reflect lt))

extremElemOf :: Ord a => ([a] -> a) -> BinTree a -> a
extremElemOf f EmptyBT = error "Empty Tree"
extremElemOf f (NodeBT val EmptyBT EmptyBT) = val
extremElemOf f (NodeBT val EmptyBT rt)      = f [val, extremElemOf f rt]
extremElemOf f (NodeBT val lt EmptyBT)      = f [extremElemOf f lt, val]
extremElemOf f (NodeBT val lt rt)           = f [extremElemOf f lt, val, extremElemOf f rt]

minElemOf :: Ord a => BinTree a -> a
minElemOf = extremElemOf minimum

maxElemOf :: Ord a => BinTree a -> a
maxElemOf = extremElemOf maximum

-- With stack help
foldBinTree :: (a -> b -> b -> b) -> b -> BinTree a -> b -- fold dla drzewa binarnego
foldBinTree _ base EmptyBT = base
foldBinTree f base (NodeBT val lt rt) = f val (foldBinTree f base lt) (foldBinTree f base rt)

-- 3
mapBT' :: (a -> b) -> BinTree a -> BinTree b
mapBT' f = foldBinTree (\val lt rt -> NodeBT (f val) lt rt) EmptyBT

-- 4
zipBT :: BinTree a -> BinTree a -> BinTree (a, a)
zipBT EmptyBT _ = EmptyBT
zipBT _ EmptyBT = EmptyBT
zipBT (NodeBT val1 lt1 rt1) (NodeBT val2 lt2 rt2) =
  NodeBT (val1, val2) (zipBT lt1 lt2) (zipBT rt1 rt2)

-- 5
data GTree a = Leaf a |
               GNode [GTree a]
               deriving Show

sumGTree :: Num a => GTree a -> a
sumGTree (Leaf val) = val
sumGTree (GNode []) = 0
sumGTree (GNode (x:xs)) = sumGTree x + sumGTree (GNode xs)

elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree a (Leaf val) = a == val
elemOfGTree _ (GNode []) = False
elemOfGTree a (GNode (x:xs)) = elemOfGTree a x || elemOfGTree a (GNode xs)

depthOfGTree :: GTree a -> Int
depthOfGTree (Leaf _) = 1
depthOfGTree (GNode []) = 0
depthOfGTree (GNode (x:xs)) = max (1 + depthOfGTree x) (depthOfGTree (GNode xs))

-- let x = GNode [Leaf 5, GNode [Leaf 1, Leaf 2, GNode [Leaf 2]], Leaf 31, GNode [Leaf 12]]

-- That was hard:
mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf val) = Leaf (f val)
mapGTree f (GNode []) = GNode []
mapGTree f (GNode (x:xs)) = connectGTree (mapGTree f x) (mapGTree f (GNode xs))

connectGTree :: GTree a -> GTree a -> GTree a
connectGTree (Leaf l1) (Leaf l2) = GNode ([Leaf l1] ++ [Leaf l2])
connectGTree (Leaf l1) (GNode g2) = GNode ([Leaf l1] ++ g2)
connectGTree (GNode g1) (Leaf l2) = GNode ([GNode g1] ++ [Leaf l2])
connectGTree (GNode g1) (GNode g2) = GNode ([GNode g1] ++ g2)

flattenGTree :: GTree a -> [a]
flattenGTree (Leaf val)     = [val]
flattenGTree (GNode [])     = []
flattenGTree (GNode (x:xs)) = flattenGTree x ++ flattenGTree (GNode xs)

countGTreeLeaves :: GTree a -> Int
countGTreeLeaves (Leaf _)       = 1
countGTreeLeaves (GNode [])     = 0
countGTreeLeaves (GNode (x:xs)) = countGTreeLeaves x + countGTreeLeaves (GNode xs)

-- 6
-- data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
--               Add (Expr a) (Expr a) |
--               Sub (Expr a) (Expr a) |
--               Mul (Expr a) (Expr a)
--
-- eval :: Num a => Expr a -> a
-- eval (Lit n)      = n
-- eval (Add e1 e2)  = eval e1 + eval e2
-- eval (Sub e1 e2)  = eval e1 - eval e2
-- eval (Mul e1 e2)  = eval e1 * eval e2
--
-- show' :: Show a => Expr a -> String
-- show' (Lit n)     = show n
-- show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
-- show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
-- show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

-- 7
-- data Expr a = Lit a |
--               Expr a :+: Expr a |
--               Expr a :-: Expr a |
--               Expr a :*: Expr a
--
-- eval :: Num a => Expr a -> a
-- eval (Lit n)      = n
-- eval (e1 :+: e2)  = eval e1 + eval e2
-- eval (e1 :-: e2)  = eval e1 - eval e2
-- eval (e1 :*: e2)  = eval e1 * eval e2
--
-- show' :: Show a => Expr a -> String
-- show' (Lit n)     = show n
-- show' (e1 :+: e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
-- show' (e1 :-: e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
-- show' (e1 :*: e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

-- 8
data Expr a = Lit a |
              Op Ops (Expr a) (Expr a) |
              If (BExpr a) (Expr a) (Expr a)

data Ops = Add | Sub | Mul

data BExpr a = BoolLit Bool |
               And (BExpr a) (BExpr a) |
               Or (BExpr a) (BExpr a) |
               Not (BExpr a) |
               Equal (Expr a) (Expr a) |
               Greater (Expr a) (Expr a)

eval :: (Ord a, Num a) => Expr a -> a
eval (Lit n)          = n
eval (Op Add e1 e2)   = eval e1 + eval e2
eval (Op Sub e1 e2)   = eval e1 - eval e2
eval (Op Mul e1 e2)   = eval e1 * eval e2
eval (If cond e1 e2)  = if bEval cond then eval e1 else eval e2
--
bEval :: (Num a, Ord a) => BExpr a -> Bool
bEval (BoolLit b)   = b
bEval (And a b)     = bEval a && bEval b
bEval (Or a b)      = bEval a || bEval b
bEval (Not a)       = not $ bEval a
bEval (Equal a b)   = eval a == eval b
bEval (Greater a b) = eval a > eval b
