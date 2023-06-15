{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Pouch.URL.Component.Path
    ( Path
    , Section
    , cons
    , fromList
    , fromText
    , fromWaiRequest
    , isEmpty
    , length
    , map
    , parser
    , toList
    , toText
    , uncons
    ) where

import           Data.Bifunctor             (second)
import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Functor               ((<&>))
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified GHC.Exts
import qualified Pouch.Parsec                 as Parsec
import           Pouch.URL.Component.Internal (componentTextParser, escapeText,
                                             unescapeText)
import qualified Network.Wai                as Wai
import           Prelude                    hiding (length, map)
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Path

newtype Path
  = Path [Section]
  deriving (Eq)

instance Show Path where
  show = Text.unpack . toText

instance GHC.Exts.IsList Path where
  type Item Path = Section
  fromList = Path
  toList (Path fragments) = fragments

instance Binary Path where
  put (Path fragments) = do
    Binary.putWord8 0
    Binary.put fragments
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Path <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Creates an empty 'Path' with no 'Section's.
empty :: Path
empty = Path []

-- | Creates a 'Path' from a list of 'Section's.
fromList :: [Section] -> Path
fromList = Path

-- | Creates a 'Path' from the path in a 'Wai.Request'. This function unescapes
-- the path from the request.
fromWaiRequest :: Wai.Request -> Path
fromWaiRequest = unescape . Path . Wai.pathInfo

-- | Parses a path string into a 'Path' and unescapes all sections. The supplied
-- 'Text' can optionally include a leading @/@ character. This function returns 'Nothing'
-- if the parse fails.
fromText :: Text -> Maybe Path
fromText = Parsec.parseMaybe parser "Path"

-- ** Conversion

-- | Converts a 'Path' into a list of 'Section's. This function does not perform any
-- escaping or unescaping.
toList :: Path -> [Section]
toList (Path fragments) = fragments

-- | Converts a 'Path' to its 'Text' representation delimited by @/@ characters.
-- The resulting path is also prepended with a @/@ character.
-- This function escapes all 'Section's.
toText :: Path -> Text
toText = Text.cons '/' . Text.intercalate "/" . toList . escape

-- ** Introspection

isEmpty :: Path -> Bool
isEmpty = (== Path [])

length :: Path -> Word
length = fromIntegral . List.length . toList

uncons :: Path -> Maybe (Section, Path)
uncons = fmap (second fromList) . List.uncons . toList

-- ** Modification

cons :: Section -> Path -> Path
cons fragment = fromList . (:) fragment . toList

-- ** Helpers

escape :: Path -> Path
escape = map escapeText

unescape :: Path -> Path
unescape = map unescapeText

-- | Maps over a the 'Section's in a 'Path'.
map :: (Section -> Section) -> Path -> Path
map f = fromList . List.map f . toList

-- | A parsec 'Parser' to parse a 'Path' in the format @/foo/bar@.
-- The parser only succeeds with a leading slash is present.
-- This parser unescapes all 'Section's and uses 'componentTextParser' in its implementation.
-- Parsed 'Section's are allowed to be empty.
parser :: Parser Path
parser = slash
      >> Parsec.sepBy section slash
     <&> (unescape . fromList)
  where
    slash = Parsec.char '/'
    section = componentTextParser

-- * Sections

type Section = Text
