module Pouch.URL.Component.Internal
    ( componentCharParser
    , componentTextParser
    , componentTextParser1
    , escapeText
    , unescapeText
    ) where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Network.URI      as URI
import qualified Text.Parsec      as Parsec
import           Text.Parsec.Text (Parser)

escapeText :: Text -> Text
escapeText = Text.pack . URI.escapeURIString URI.isUnreserved . Text.unpack

unescapeText :: Text -> Text
unescapeText = Text.pack . URI.unEscapeString . Text.unpack

componentCharParser :: Parser Char
componentCharParser = Parsec.noneOf [' ', '\n', '\t', '/', '=', '?', ':', '@', '#', '&', '[', ']']

componentTextParser :: Parser Text
componentTextParser = Text.pack <$> Parsec.many componentCharParser

componentTextParser1 :: Parser Text
componentTextParser1 = Text.pack <$> Parsec.many1 componentCharParser
