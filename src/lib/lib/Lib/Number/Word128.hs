module Lib.Number.Word128
    ( Word128
    ) where

import qualified Basement.Numerical.Number as Number
import qualified Basement.Types.Word128    as Word128
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Bits                 (Bits)
import           Data.Ratio                ((%))
import           Foreign.Storable          (Storable)

-- | Wrap the upstream 'Word128.Word128' type from basement
-- to avoid any typeclass instance collisions.
newtype Word128
  = Word128 Word128.Word128
  deriving (Bits, Bounded, Enum, Eq, Num, Ord, Show, Storable)

instance Binary Word128 where
  get = do
    a <- Binary.get
    b <- Binary.get
    return $ Word128 $ Word128.Word128 a b
  put (Word128 (Word128.Word128 a b)) = Binary.put a >> Binary.put b

instance Real Word128 where
  toRational (Word128 w) = Number.toInteger w % 1

instance Integral Word128 where
  toInteger (Word128 w) = Number.toInteger w
  quotRem (Word128 a) (Word128 b) = (Word128 (Word128.quot a b), Word128 (Word128.rem a b))

