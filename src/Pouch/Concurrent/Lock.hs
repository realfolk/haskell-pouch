module Pouch.Concurrent.Lock
    ( Lock
    , acquire
    , new
    , newAcquired
    , release
    ) where

import qualified Control.Concurrent.MVar as MVar

newtype Lock
  = Lock (MVar.MVar ())

new :: IO Lock
new = Lock <$> MVar.newMVar ()

newAcquired :: IO Lock
newAcquired = Lock <$> MVar.newEmptyMVar

acquire :: Lock -> IO ()
acquire (Lock x) = MVar.takeMVar x

release :: Lock -> IO ()
release (Lock x) = MVar.putMVar x ()
