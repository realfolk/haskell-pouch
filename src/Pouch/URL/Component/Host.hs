{-# LANGUAGE OverloadedStrings #-}

module Pouch.URL.Component.Host
    ( Host
    , fromIPv4
    , fromIPv6
    , fromName
    , fromText
    , isIPv4
    , isIPv6
    , isName
    , parser
    , toText
    ) where

import           Control.Applicative          ((<|>))
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Binary
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Pouch.Parsec                   as Parsec
import qualified Pouch.URL.Component.Host.IP.V4 as IPv4
import qualified Pouch.URL.Component.Host.IP.V6 as IPv6
import           Pouch.URL.Component.Internal   (componentTextParser1, escapeText,
                                               unescapeText)
import qualified Text.Parsec                  as Parsec
import           Text.Parsec.Text             (Parser)

-- * Types

data Host
  = Name !Char !Text
  | IPv4 !IPv4.Address
  | IPv6 !IPv6.Address
  deriving (Eq)

instance Show Host where
  show = Text.unpack . toText False

instance Binary Host where
  put host =
    case host of
      Name head rest -> Binary.putWord8 0 >> Binary.put head >> Binary.put rest
      IPv4 a         -> Binary.putWord8 1 >> Binary.put a
      IPv6 a         -> Binary.putWord8 2 >> Binary.put a
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Name <$> Binary.get <*> Binary.get
      1 -> IPv4 <$> Binary.get
      2 -> IPv6 <$> Binary.get
      _ -> fail "Invalid ID"

-- * Constructors

fromName :: Text -> Maybe Host
fromName = fmap (uncurry Name) . Text.uncons

fromIPv4 :: IPv4.Address -> Host
fromIPv4 = IPv4

fromIPv6 :: IPv6.Address -> Host
fromIPv6 = IPv6

fromText :: Bool -> Text -> Maybe Host
fromText includeIPv6Brackets = Parsec.parseMaybe (parser includeIPv6Brackets) "Host"

-- * Conversion

toText :: Bool -> Host -> Text
toText includeIPv6Brackets host =
  case host of
    Name head rest -> escapeText $ Text.cons head rest
    IPv4 a         -> IPv4.toText a
    IPv6 a         -> IPv6.toText includeIPv6Brackets a

-- * Introspection

-- | Returns 'True' if the supplied 'Host' is a name.
isName :: Host -> Bool
isName host =
  case host of
    Name _ _ -> True
    _        -> False

-- | Returns 'True' if the supplied 'Host' is an IPv4 address.
isIPv4 :: Host -> Bool
isIPv4 host =
  case host of
    IPv4 _ -> True
    _      -> False

-- | Returns 'True' if the supplied 'Host' is a an IPv6 Address.
isIPv6 :: Host -> Bool
isIPv6 host =
  case host of
    IPv6 _ -> True
    _      -> False

-- ** Helpers

parser :: Bool -> Parser Host
parser includeIPv6Brackets = fmap IPv4 IPv4.parser
                         <|> fmap IPv6 (IPv6.parser includeIPv6Brackets)
                         <|> name
  where
    name = do
      text <- componentTextParser1
      case Text.uncons (unescapeText text) of
        Just (head, rest) -> return $ Name head rest
        Nothing           -> fail "Host Name is an empty string."
