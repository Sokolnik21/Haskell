module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  ) where

-- interface
emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

-- implementation
newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue list) = null list
addQ val (MkQueue list) = MkQueue (val:list)
remQ (MkQueue (h:list))   = (h, MkQueue list)
