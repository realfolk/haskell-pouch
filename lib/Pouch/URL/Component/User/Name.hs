{-# LANGUAGE OverloadedStrings #-}

module Pouch.URL.Component.User.Name
    ( Name
    , fromText
    , parser
    , toText
    ) where

import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified GHC.Exts
import qualified Pouch.Parsec                 as Parsec
import           Pouch.URL.Component.Internal (componentTextParser, escapeText,
                                             unescapeText)
import qualified Text.Parsec                as Parsec
import           Text.Parsec.Text           (Parser)

-- * Name

newtype Name
  = Name Text
  deriving (Eq, GHC.Exts.IsString)

instance Show Name where
  show = Text.unpack . toText

instance Binary Name where
  put (Name name) =
    Binary.putWord8 0 >> Binary.put name
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Name <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Parses 'Text' into a 'Name'. This function returns 'Nothing' when the supplied
-- the parse to fails. The 'Text' is unescaped when creating the 'Name'.
fromText :: Text -> Maybe Name
fromText = Parsec.parseMaybe parser "User Name"

-- ** Conversion

-- | Converts a 'Name' to 'Text'. The resulting 'Text' is escaped.
toText :: Name -> Text
toText (Name name) = escapeText name

-- ** Helpers

parser :: Parser Name
parser = (Name . unescapeText) <$> componentTextParser
