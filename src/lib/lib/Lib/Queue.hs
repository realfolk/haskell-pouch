{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib.Queue
    ( Queue
    , dequeue
    , empty
    , enqueue
    , isEmpty
    , singleton
    ) where

import qualified Data.Binary  as B
import           GHC.Generics (Generic, Generic1)

-- Efficient queue implementation based on Chris Okasaki's thesis on Purely Functional Data Structures (http://www.cs.cmu.edu/%7Erwh/theses/okasaki.pdf)

data Queue a
  = Queue [a] [a]
  deriving (Show)

deriving instance Generic1 Queue
deriving instance Generic (Queue a)

instance B.Binary a => B.Binary (Queue a) where
  put (Queue xs ys) = do
    B.put xs
    B.put ys
  get = Queue <$> B.get <*> B.get

empty :: Queue a
empty = Queue [] []

singleton :: a -> Queue a
singleton x = Queue [] [x]

-- | Test whether a 'Queue' is empty.
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- | Add an item to the queue.
enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs ys) item = Queue xs (item:ys)

-- | Remove an item from the queue and return it in an either:
-- Left value contains the original (empty) 'Queue' and signifies an error condition.
-- Right value contains a tuple with the dequeued value and the updated 'Queue'.
dequeue :: Queue a -> Either (Queue a) (a, Queue a)
dequeue q =
  case q of
    Queue [] []     -> Left q
    Queue (x:xs) ys -> Right (x, Queue xs ys)
    Queue [] ys     -> dequeue (Queue (reverse ys) [])
