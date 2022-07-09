module Lib.Number.Word256
    ( Word256
    ) where

import qualified Basement.Numerical.Number as Number
import qualified Basement.Types.Word256    as Word256
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Bits                 (Bits)
import           Data.Ratio                ((%))
import           Foreign.Storable          (Storable)

-- | Wrap the upstream 'Word256.Word256' type from basement
-- to avoid any typeclass instance collisions.
newtype Word256
  = Word256 Word256.Word256
  deriving (Bits, Bounded, Enum, Eq, Num, Ord, Storable)

instance Show Word256 where
  show (Word256 w) = show w

instance Binary Word256 where
  get = do
    a <- Binary.get
    b <- Binary.get
    c <- Binary.get
    d <- Binary.get
    return $ Word256 $ Word256.Word256 a b c d
  put (Word256 (Word256.Word256 a b c d)) = Binary.put a >> Binary.put b >> Binary.put c >> Binary.put d

instance Real Word256 where
  toRational (Word256 w) = Number.toInteger w % 1

instance Integral Word256 where
  toInteger (Word256 w) = Number.toInteger w
  quotRem (Word256 a) (Word256 b) = (Word256 (Word256.quot a b), Word256 (Word256.rem a b))
