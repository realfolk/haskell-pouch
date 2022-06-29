{-# LANGUAGE OverloadedStrings #-}

module Lib.URL.Component.Host.IP.V6
    ( AbbreviatedAddress
    , Address
    , Hexadectet
    , Hexadectets
    , abbreviate
    , fromHexadectets
    , fromText
    , fromWord128
    , loopback
    , parser
    , toHexadectets
    , toText
    , toWord128
    , unabbreviate
    ) where

import qualified Basement.Numerical.Number  as Number
import           Basement.Types.Word128     (Word128 (Word128))
import           Control.Applicative        ((<|>))
import           Control.Monad              (unless, void, when)
import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Function              ((&))
import           Data.Functor               (($>), (<&>))
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import           Data.Word                  (Word16, Word8)
import qualified GHC.Bits                   as Bits
import qualified Lib.Parsec                 as Parsec
import           Lib.URL.Component.Internal (escapeText, unescapeText)
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Address

newtype Address
  = Address Word128
  deriving (Eq)

instance Show Address where
  show = Text.unpack . toText False

instance Binary Address where
  put (Address (Word128 w0 w1)) = Binary.putWord8 0 >> Binary.put w0 >> Binary.put w1
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Word128 <$> Binary.get <*> Binary.get <&> Address
      _ -> fail "Invalid ID"

-- ** Constructors

fromWord128 :: Word128 -> Address
fromWord128 = Address

fromHexadectets :: Hexadectets -> Address
fromHexadectets (h0, h1, h2, h3, h4, h5, h6, h7) =
  Address
    ( convertHexadectet h0 H0
    + convertHexadectet h1 H1
    + convertHexadectet h2 H2
    + convertHexadectet h3 H3
    + convertHexadectet h4 H4
    + convertHexadectet h5 H5
    + convertHexadectet h6 H6
    + convertHexadectet h7 H7
    )
    where
      convertHexadectet hexadectet index =
        Bits.shift (fromIntegral hexadectet) (word128BitSize - ((word128HexadectetIndexToInt index) + 1) * hexadectetBitSize)

fromText :: Bool -> Text -> Maybe Address
fromText includeBrackets = Parsec.parseMaybe (parser includeBrackets) "IPv6 Address"

loopback :: Address
loopback = fromHexadectets (0, 0, 0, 0, 0, 0, 0, 1)

-- ** Conversion

toWord128 :: Address -> Word128
toWord128 (Address n) = n

toHexadectets :: Address -> Hexadectets
toHexadectets (Address n) =
  ( getHexadectet H0
  , getHexadectet H1
  , getHexadectet H2
  , getHexadectet H3
  , getHexadectet H4
  , getHexadectet H5
  , getHexadectet H6
  , getHexadectet H7
  )
    where
      getHexadectet = getWord128Hexadectet n

toText :: Bool -> Address -> Text
toText includeBrackets address =
  abbreviate address
    & makeBuilder
    & optionallyAddBrackets
    & Builder.toLazyText
    & LazyText.toStrict
    where
      makeBuilder abbreviated =
        case abbreviated of
          NoCompressedZeroes suffix ->
            makeHexadecimalBuilders suffix
          WithCompressedZeroes prefix suffix ->
            makeHexadecimalBuilders prefix <> colon <> colon <> makeHexadecimalBuilders suffix
      makeHexadecimalBuilders = mconcat . List.intersperse colon . map Builder.hexadecimal
      colon = Builder.singleton ':'
      optionallyAddBrackets t =
        if includeBrackets
           then Builder.singleton '[' <> t <> Builder.singleton ']'
           else t

-- ** Helpers

parser :: Bool -> Parser Address
parser includeBrackets = do
  when includeBrackets $ void $ Parsec.char '['
  abbreviated <- abbreviatedAddress (NoCompressedZeroes [])
  when includeBrackets $ void $ Parsec.char ']'
  maybe (fail "Unable to unabbreviate address.") return $ unabbreviate abbreviated
  where
    hexadectet :: Parser Hexadectet
    hexadectet = Parsec.hexadecimal
    colon :: Parser ()
    colon = void $ Parsec.char ':'
    compressedZeroes :: Parser ()
    compressedZeroes = colon >> colon
    abbreviatedAddress :: AbbreviatedAddress -> Parser AbbreviatedAddress
    abbreviatedAddress acc =
      case acc of
        NoCompressedZeroes prefix ->
          let a = do Parsec.try compressedZeroes
                     abbreviatedAddress (WithCompressedZeroes prefix [])
              b = do unless (List.null prefix) colon
                     h <- hexadectet
                     abbreviatedAddress (NoCompressedZeroes (prefix <> [h]))
              c = return acc
          in a <|> b <|> c
        WithCompressedZeroes prefix suffix ->
          let a = do unless (List.null suffix) colon
                     h <- hexadectet
                     abbreviatedAddress (WithCompressedZeroes prefix (suffix <> [h]))
              b = return acc
          in a <|> b

-- * Hexadectets

type Hexadectets = (Hexadectet, Hexadectet, Hexadectet, Hexadectet, Hexadectet, Hexadectet, Hexadectet, Hexadectet)

type Hexadectet = Word16

hexadectetBitSize = 16

-- * Address Abbreviation

data AbbreviatedAddress
  = NoCompressedZeroes [Hexadectet]
  | WithCompressedZeroes [Hexadectet] [Hexadectet]
  deriving (Eq, Show)

abbreviate :: Address -> AbbreviatedAddress
abbreviate address =
  fst $ foldr f (NoCompressedZeroes [], 0) [h0, h1, h2, h3, h4, h5, h6, h7]
    where
      (h0, h1, h2, h3, h4, h5, h6, h7) = toHexadectets address
      f hexadectet (abbreviated, zeroCount) =
        case (hexadectet, abbreviated, zeroCount) of
          -- Start counting zeroes. Count starts at one (which means one zero has occurred).
          (0, NoCompressedZeroes suffix, _) -> (WithCompressedZeroes [] suffix, 1)
          -- Continue counting zeroes.
          (0, WithCompressedZeroes [] suffix, _) -> (WithCompressedZeroes [] suffix, zeroCount + 1)
          -- Revert counting zeroes if only counted one (spec demands only 2+ zeroes are compressed).
          (_, WithCompressedZeroes [] suffix, 1) -> (NoCompressedZeroes (hexadectet : 0 : suffix), 0)
          -- Simply add the next hexadectet if non-zero or zero but outside the completed compression block.
          (_, WithCompressedZeroes prefix suffix, _) -> (WithCompressedZeroes (hexadectet : prefix) suffix, zeroCount)
          (_, NoCompressedZeroes suffix, _) -> (NoCompressedZeroes (hexadectet : suffix), zeroCount)


unabbreviate :: AbbreviatedAddress -> Maybe Address
unabbreviate address =
  case address of
    NoCompressedZeroes [h0, h1, h2, h3, h4, h5, h6 ,h7]
      -> Just $ fromHexadectets (h0, h1, h2, h3, h4, h5, h6, h7)
    WithCompressedZeroes prefix suffix
      -> let zeroes = replicate (max 0 (8 - length prefix - length suffix)) 0
         in case prefix <> zeroes <> suffix of
           [h0, h1, h2, h3, h4, h5, h6 ,h7]
             -> Just $ fromHexadectets (h0, h1, h2, h3, h4, h5, h6, h7)
           _ -> Nothing
    _ -> Nothing

-- * Hexadectet Indexing

-- | Represents the index of a hexadectet (i.e. byte) in a 'Word128'.
-- The value constructors are indexed from 0 to 7, representing the eight hexadectets
-- that compose a 128-bit number. The indexes are ordered from left to right, so
-- 'H0' represents the left-most hexadectet. Associated functions assume big-endianness.
data Word128HexadectetIndex = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7

word128BitSize :: Int
word128BitSize = 128

word128HexadectetIndexToInt :: Word128HexadectetIndex -> Int
word128HexadectetIndexToInt index =
  case index of
    H0 -> 0
    H1 -> 1
    H2 -> 2
    H3 -> 3
    H4 -> 4
    H5 -> 5
    H6 -> 6
    H7 -> 7

getWord128Hexadectet :: Word128 -> Word128HexadectetIndex -> Hexadectet
getWord128Hexadectet n index =
  fromInteger
    $ Number.toInteger
    $ Bits.shift
        (Bits.shift n ((word128HexadectetIndexToInt index) * hexadectetBitSize))
        (word128BitSize * (-1) + hexadectetBitSize)
