module Lib.Number.Word256
    ( module Basement.Types.Word256
    ) where

import           Basement.Types.Word256 (Word256 (Word256))
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary

instance Binary Word256 where
  get = Word256 <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
  put (Word256 a b c d) = Binary.put a >> Binary.put b >> Binary.put c >> Binary.put d
