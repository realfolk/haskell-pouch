module Lib.Parsec
    ( decimal
    , hexadecimal
    , parseMaybe
    , parseMaybeT
    ) where

import           Control.Comonad.Identity (Identity)
import qualified Data.Char                as Char
import qualified Lib.Either               as Either
import qualified Text.Parsec              as Parsec
import           Text.Parsec              (Parsec, ParsecT, SourceName, Stream)

parseMaybeT :: Stream s m t => ParsecT s u m a -> u -> SourceName -> s -> m (Maybe a)
parseMaybeT parser state name stream = Either.toMaybe <$> Parsec.runParserT parser state name stream

parseMaybe :: Stream s Identity t => Parsec s () a -> SourceName -> s -> Maybe a
parseMaybe parser name stream = Either.toMaybe $ Parsec.parse parser name stream

decimal :: (Integral a, Stream s m Char) => ParsecT s u m a
decimal = computeIntegral 10 <$> Parsec.many1 Parsec.digit

hexadecimal :: (Integral a, Stream s m Char) => ParsecT s u m a
hexadecimal = computeIntegral 16 <$> Parsec.many1 Parsec.hexDigit

computeIntegral :: Integral a => a -> [Char] -> a
computeIntegral base digits =
  foldl (\sum digit -> sum * base + fromIntegral (Char.digitToInt digit)) 0 digits
