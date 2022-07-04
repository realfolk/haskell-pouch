module Lib.Number.Word128
    ( module Basement.Types.Word128
    ) where

import           Basement.Types.Word128 (Word128 (Word128))
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary

instance Binary Word128 where
  get = Word128 <$> Binary.get <*> Binary.get
  put (Word128 a b) = Binary.put a >> Binary.put b
