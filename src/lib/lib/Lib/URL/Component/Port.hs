module Lib.URL.Component.Port
    ( Port
    , fromText
    , fromWord16
    , parser
    , toText
    , toWord16
    ) where

import           Data.Binary      (Binary)
import qualified Data.Binary      as Binary
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Word        (Word16)
import qualified Lib.Parsec       as Parsec
import qualified Text.Parsec      as Parsec
import           Text.Parsec.Text (Parser)

-- * Port

newtype Port
  = Port Word16
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show Port where
  show = Text.unpack . toText

instance Binary Port where
  put (Port n) =
    Binary.putWord8 0 >> Binary.put n
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Port <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Creates a 'Port' from a 'Word16'.
fromWord16 :: Word16 -> Port
fromWord16 = Port

-- | Creates a 'Port' from a 'Text'.
fromText :: Text -> Maybe Port
fromText = Parsec.parseMaybe parser "Port"

-- ** Conversion

-- | Converts a 'Port' to 'Word16'.
toWord16 :: Port -> Word16
toWord16 (Port n) = n

-- | Converts a 'Port' to 'Text'.
toText :: Port -> Text
toText = Text.pack . show . toWord16

-- ** Helpers

-- | A parsec 'Parser' to parse a 'Port'.
parser :: Parser Port
parser = Parsec.decimal
