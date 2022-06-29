module Lib.Ref
    ( Ref
    , get
    , new
    , set
    ) where

import qualified Control.Concurrent.MVar as MVar
import           Control.Monad           (void)

newtype Ref a
  = Ref (MVar.MVar a)

new :: a -> IO (Ref a)
new a = Ref <$> MVar.newMVar a

get :: Ref a -> IO a
get (Ref x) = MVar.readMVar x

set :: Ref a -> a -> IO ()
set (Ref x) = void . MVar.swapMVar x
