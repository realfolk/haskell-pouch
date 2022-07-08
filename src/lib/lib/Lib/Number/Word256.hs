module Lib.Number.Word256
    ( module Basement.Types.Word256
    ) where

import qualified Basement.Numerical.Number as Number
import           Basement.Types.Word256    (Word256 (Word256))
import qualified Basement.Types.Word256    as Word256
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Ratio                ((%))


instance Binary Word256 where
  get = Word256 <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
  put (Word256 a b c d) = Binary.put a >> Binary.put b >> Binary.put c >> Binary.put d

instance Real Word256 where
  toRational w = Number.toInteger w % 1

instance Integral Word256 where
  toInteger = Number.toInteger
  quotRem a b = (Word256.quot a b, Word256.rem a b)

