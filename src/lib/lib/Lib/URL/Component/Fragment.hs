{-# LANGUAGE OverloadedStrings #-}

module Lib.URL.Component.Fragment
    ( Fragment
    , empty
    , fromText
    , isEmpty
    , parser
    , toText
    ) where

import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified GHC.Exts
import qualified Lib.Parsec                 as Parsec
import           Lib.URL.Component.Internal (componentTextParser, escapeText,
                                             unescapeText)
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Fragment

newtype Fragment
  = Fragment Text
  deriving (Eq, GHC.Exts.IsString)

instance Show Fragment where
  show = Text.unpack . toText

instance Binary Fragment where
  put (Fragment s) =
    Binary.putWord8 0 >> Binary.put s
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Fragment <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Creates an empty 'Fragment'.
empty :: Fragment
empty = Fragment ""

-- | Creates a 'Fragment' from 'Text'. The 'Text' is unescaped when creating the 'Fragment'.
fromText :: Text -> Maybe Fragment
fromText = Parsec.parseMaybe parser "Fragment"

-- ** Conversion

-- | Converts a 'Fragment' to 'Text'. The resulting 'Text' is escaped.
toText :: Fragment -> Text
toText (Fragment s) = escapeText s

-- ** Introspection

-- | Returns 'True' if the supplied 'Fragment' is empty.
isEmpty :: Fragment -> Bool
isEmpty = (==) ""

-- ** Helpers

parser :: Parser Fragment
parser = (Fragment . unescapeText) <$> componentTextParser
