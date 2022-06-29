{-# LANGUAGE OverloadedStrings #-}

module Lib.URL.Component.User.Password
    ( Password
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

-- * Password

newtype Password
  = Password Text
  deriving (Eq, GHC.Exts.IsString)

instance Show Password where
  show = Text.unpack . toText

instance Binary Password where
  put (Password pw) =
    Binary.putWord8 0 >> Binary.put pw
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Password <$> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Creates an empty 'Password'.
empty :: Password
empty = Password ""

-- | Creates a 'Password' from 'Text'. The 'Text' is unescaped when creating the 'Password'.
fromText :: Text -> Maybe Password
fromText = Parsec.parseMaybe parser "User Password"

-- ** Conversion

-- | Converts a 'Password' to 'Text'. The resulting 'Text' is escaped.
toText :: Password -> Text
toText (Password pw) = escapeText pw

-- ** Introspection

-- | Returns 'True' if the supplied 'Password' is empty.
isEmpty :: Password -> Bool
isEmpty = (==) empty

-- ** Helpers

parser :: Parser Password
parser = (Password . unescapeText) <$> componentTextParser
