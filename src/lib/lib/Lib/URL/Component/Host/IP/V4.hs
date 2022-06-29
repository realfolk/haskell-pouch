{-# LANGUAGE OverloadedStrings #-}

module Lib.URL.Component.Host.IP.V4
    ( Address
    , fromOctets
    , fromText
    , fromWord32
    , loopback
    , parser
    , toOctets
    , toText
    , toWord32
    ) where

import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Word                  (Word32, Word8)
import qualified GHC.Bits                   as Bits
import qualified Lib.Parsec                 as Parsec
import           Lib.URL.Component.Internal (escapeText, unescapeText)
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Types

-- ** Address

newtype Address
  = Address Word32
  deriving (Eq)

instance Show Address where
  show = Text.unpack . toText

instance Binary Address where
  put (Address n) = Binary.putWord8 0 >> Binary.put n
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Address <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Octets

type Octets = (Octet, Octet, Octet, Octet)

type Octet = Word8

octetBitSize = 8

-- * Constructors

fromWord32 :: Word32 -> Address
fromWord32 = Address

fromOctets :: Octets -> Address
fromOctets (o0, o1, o2, o3) =
  Address (convertOctet o0 O0 + convertOctet o1 O1 + convertOctet o2 O2 + convertOctet o3 O3)
    where
      convertOctet octet index =
        Bits.shift (fromIntegral octet) (word32BitSize - ((word32OctetIndexToInt index) + 1) * octetBitSize)

fromText :: Text -> Maybe Address
fromText = Parsec.parseMaybe parser "IPv4 Address"

loopback :: Address
loopback = fromOctets (127, 0, 0, 1)

-- * Conversion

toWord32 :: Address -> Word32
toWord32 (Address n) = n

toOctets :: Address -> Octets
toOctets (Address n) = (getOctet O0, getOctet O1, getOctet O2, getOctet O3)
  where
    getOctet = getWord32Octet n

toText :: Address -> Text
toText address =
  Text.intercalate "." [octetToText o0, octetToText o1, octetToText o2, octetToText o3]
    where
      (o0, o1, o2, o3) = toOctets address
      octetToText = Text.pack . show

-- * Helpers

parser :: Parser Address
parser = do
  o1 <- Parsec.decimal
  Parsec.char '.'
  o2 <- Parsec.decimal
  Parsec.char '.'
  o3 <- Parsec.decimal
  Parsec.char '.'
  o4 <- Parsec.decimal
  return $ fromOctets (o1, o2, o3, o4)

-- ** Octet Indexing

-- | Represents the index of an octet (i.e. byte) in a 'Word32'.
-- The value constructors are indexed from 0 to 3, representing the four octets
-- that compose a 32-bit number. The indexes are ordered from left to right, so
-- 'O0' represents the left-most octet. Associated functions assume big-endianness.
data Word32OctetIndex = O0 | O1 | O2 | O3

word32BitSize :: Int
word32BitSize = 32

word32OctetIndexToInt :: Word32OctetIndex -> Int
word32OctetIndexToInt index =
  case index of
    O0 -> 0
    O1 -> 1
    O2 -> 2
    O3 -> 3

getWord32Octet :: Word32 -> Word32OctetIndex -> Octet
getWord32Octet n index =
  fromIntegral
    $ Bits.shift
        (Bits.shift n ((word32OctetIndexToInt index) * octetBitSize))
        (word32BitSize * (-1) + octetBitSize)
