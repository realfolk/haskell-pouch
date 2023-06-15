{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Pouch.URL.Component.Query.Item
-- Description: A library for interacting with key-pair values in URL queries.
-- Copyright: (c) Real Folk Inc. 2022
-- Maintainer: admin@realfolk.com
-- Stability: experimental
-- Portability: POSIX
--
-- This library includes a type, 'Item', for representing key-value pairs in URL queries, and associated functions.

module Pouch.URL.Component.Query.Item
    ( Item
    , fromNetworkQueryItem
    , fromText
    , parser
    , toNetworkQueryItem
    , toText
    ) where

import           Control.Applicative        ((<|>))
import           Data.Bifunctor             (bimap)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Pouch.Parsec                 as Parsec
import           Pouch.URL.Component.Internal (componentTextParser1, escapeText,
                                             unescapeText)
import qualified Network.HTTP.Types.URI     as URI
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Item

-- | An 'Item' is simply a key-value pair represented as @('Text', 'Text')@.
type Item = (Text, Text)

-- ** Constructors

-- | Creates an 'Item' from a 'URI.QueryItem' for compatibility. This function unescapes
-- the provided key and value so the resulting 'Item' is unescaped.
fromNetworkQueryItem :: URI.QueryItem -> Item
fromNetworkQueryItem = unescape . bimap decodeUtf8 (maybe "" decodeUtf8)

-- | Parses a query string item into an 'Item' and unescapes the key and value.
-- This function returns 'Nothing' if the parse fails.
fromText :: Text -> Maybe Item
fromText = Parsec.parseMaybe parser "Item"

-- ** Conversion

-- | Converts an 'Item' into a 'URI.QueryItem' for compatibility. This function escapes
-- the key and value.
toNetworkQueryItem :: Item -> URI.QueryItem
toNetworkQueryItem = bimap encodeUtf8 encodeValue . escape
  where
    encodeValue "" = Nothing
    encodeValue v' = Just $ encodeUtf8 v'

-- | Converts an 'Item' to its query string representation as 'Text'.
-- This function escapes the key and value.
toText :: Item -> Text
toText = toText' . escape
  where
    toText' (key, value) = key <> equals value
    equals value = if value == "" then "" else "=" <> value

-- ** Helpers

-- | Unescapes an 'Item' without converting it to another type.
unescape :: Item -> Item
unescape = bimap unescapeText unescapeText

-- | Escapes an 'Item' without converting it to another type.
escape :: Item -> Item
escape = bimap escapeText escapeText

-- | A parsec 'Parser' to parse an 'Item' in the format @key=value@.
-- This function uses 'componentTextParser1' to parse the @key@ and @value@.
-- This parser unescapes the key and value.
parser :: Parser Item
parser = do
  key <- componentTextParser1
  value <- (Parsec.char '=' >> componentTextParser1) <|> return ""
  return $ unescape (key, value)
