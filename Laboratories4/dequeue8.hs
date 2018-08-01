module Dequeue
 ( Dequeue
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int, O(1)
 , firstDEQ     -- :: Dequeue a -> Maybe a,  O(1)
 , lastDEQ      -- :: Dequeue a -> Maybe a, O(1)
 , takeFrontDEQ -- :: Int -> Dequeue a -> [a], O(n)
 , takeBackDEQ  -- :: Int -> Dequeue a -> [a], O(n)
 , pushFrontDEQ -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popFrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, q a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 ) where

-- interface
emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

-- implementation
newtype Dequeue a = MkDequeue ([a], [a]) --deriving Show

instance Show a => Show (Dequeue a) where
  show (MkDequeue (a, _)) = "MkDequeue " ++ show a

emptyDEQ = MkDequeue ([], [])
isEmptyDEQ (MkDequeue (list1, _))     = null list1
lengthDEQ (MkDequeue (list1, _))      = length list1
firstDEQ (MkDequeue ([], []))         = Nothing
firstDEQ (MkDequeue (list1, _))       = Just (head list1)
lastDEQ (MkDequeue ([], []))          = Nothing
lastDEQ (MkDequeue (_, list2))        = Just (head list2)
takeFrontDEQ n (MkDequeue (list1, _)) = take n list1
takeBackDEQ n (MkDequeue (_, list2))  = take n list2
pushFrontDEQ (MkDequeue (list1, list2)) val = MkDequeue ([val] ++ list1, list2 ++ [val])
popFrontDEQ (MkDequeue ([], []))        = Nothing
popFrontDEQ (MkDequeue (list1, list2))  = Just (head list1, MkDequeue (tail list1, take (length list2 - 1) list2))
pushBackDEQ (MkDequeue (list1, list2)) val  = MkDequeue (list1 ++ [val], [val] ++ list2)
popBackDEQ (MkDequeue ([], []))       = Nothing
popBackDEQ (MkDequeue (list1, list2)) = Just (head list2, MkDequeue ((take (length list1 - 1) list1), tail list2))
fromListDEQ list = MkDequeue (list, reverse list)

-- I thought that storing two lists would decrease an amount of time needed to
-- get first/last element, but no. It doesn't work. It's even slower than simpler
-- implementation with one list and first/last function. I think that it's
-- because of lazy evalution that enforces not to store whole list in memory,
-- only "recipe" for it. Then, when I want to get access to element, program
-- only then starts to generate lists.
