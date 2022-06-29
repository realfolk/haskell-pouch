{-|
Module: Lib.Concurrent.Queue
Description: Queue accumulative items concurrently to be automatically flushed in a forked thread on an interval.
Copyright: (c) Real Folk Inc. 2021
Maintainer: admin@realfolk.com
Stability: experimental
Portability: POSIX
-}
module Lib.Concurrent.Queue
    ( Flush
    , Queue
    , Queued
    , enqueue
    , enqueueM
    , start
    , stop
    ) where

import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad           (forever)

-- EXTERNAL

-- | A @'Queue' a@ represents an accumulative queue, where @a@ is the type of a value
-- than can be accumulated and flushed according to FIFO rules. An example instance of
-- @a@ is an 'Array'. Another example is any type that is an instance of the 'Monad' typeclass,
-- because the '(>>)' operation is an effective way to "queue" monads for execution.
--
-- This type is stateful as it stores the underlying queue in an 'MVar'. It also consists of its
-- associated 'Flush' function and the 'ThreadId' of the daemon that periodically flushes
-- its queue of items.
data Queue a
  = Queue
      { _qFlush    :: !(Flush a)
      , _qQueued   :: !(MVar (Queued a))
      , _qThreadId :: !ThreadId
      }

-- | A 'Flush' function flushes the accumulated @m@ from the queue.
type Flush a = a -> IO ()

-- | A @'Queued' a@ represents a pure, accumulative queue, used internally by 'Queue'.
-- The 'Maybe' in this type enables us to avoid unnecessarily flushing empty queues.
type Queued a = Maybe a

-- | The @'start' interval flush@ function creates a 'Queue' given an @interval@
-- in microseconds ('Int') and a 'Flush' function. A daemon is started in a forked
-- thread to flush the newly-created queue on each @interval@.
start :: Int -> Flush a -> IO (Queue a)
start interval flush = do
  queuedMVar <- MVar.newMVar Nothing
  threadId <- forkIO $ daemon interval flush queuedMVar
  return $ Queue flush queuedMVar threadId

-- | The @'stop' queue@ function stops the given 'Queue' by flushing its queue and
-- killing its daemon's thread. The 'Queue' can still enqueue items, but
-- they will not be flushed at all. After running 'stop', it is not possible
-- to resume the 'Queue'.
stop :: Queue a -> IO ()
stop  (Queue flush queuedMVar threadId) = do
  queued <- MVar.takeMVar queuedMVar
  flushWith flush queued
  killThread threadId
  MVar.putMVar queuedMVar Nothing

-- | The @'enqueue' f queue@ function appends an item to the queue with the given
-- function @f :: 'Queued' a -> a@. This function is thread-safe as the underlying queue is
-- stored in an 'MVar'.
enqueue :: (Queued a -> a) -> Queue a -> IO ()
enqueue f (Queue _ queuedMVar _) = do
  queued <- MVar.takeMVar queuedMVar
  MVar.putMVar queuedMVar $ Just $ f queued

-- | The @'enqueueM' m queue@ function appends the monadic argument @m :: m a@ to the given
-- @queue :: 'Queue' (m a)@ argument. This function uses 'enqueue' under the hood.
enqueueM :: Monad m => m a -> Queue (m a) -> IO ()
enqueueM m = enqueue (maybe m (>> m))

-- INTERNAL

-- | The @'flushWith' flush queued@ function flushes a 'Queued' to the database using a
-- 'Flush' function. If the @queued@ is 'Nothing', this function is a no-op 'IO' action.
flushWith :: Flush a -> Queued a -> IO ()
flushWith = maybe (return ())

-- | The @'daemon' interval flush queuedMVar@ function flushes a stateful 'Queued' every
-- @interval@ microseconds. This function runs infinitely in the 'IO' monad. Its intended use is in
-- a forked 'IO' thread, and can be terminated by killing its thread.
daemon :: Int -> Flush a -> MVar (Queued a) -> IO ()
daemon interval flush queuedMVar = forever $ do
  threadDelay interval
  queued <- MVar.swapMVar queuedMVar Nothing
  flushWith flush queued
