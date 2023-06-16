{-# LANGUAGE OverloadedStrings #-}

module Pouch.URL.Component.User
    ( Name
    , Password
    , User
    , fromCredentials
    , fromNameOnly
    , fromNetworkURIAuth
    , fromText
    , fromTextCredentials
    , fromTextNameOnly
    , hasNonEmptyPassword
    , parser
    , toText
    ) where

import           Control.Applicative             ((<|>))
import           Data.Binary                     (Binary)
import qualified Data.Binary                     as Binary
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Pouch.Parsec                      as Parsec
import           Pouch.URL.Component.User.Name     (Name)
import qualified Pouch.URL.Component.User.Name     as Name
import           Pouch.URL.Component.User.Password (Password)
import qualified Pouch.URL.Component.User.Password as Password
import qualified Network.URI                     as URI
import qualified Text.Parsec                     as Parsec
import           Text.Parsec.Text                (Parser)

-- * User

data User
  = User !Name !(Maybe Password)
  deriving (Eq)

instance Show User where
  show = Text.unpack . toText

instance Binary User where
  put (User name maybePw) = do
    Binary.putWord8 0
    Binary.put name
    Binary.put maybePw
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> User <$> Binary.get <*> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

-- | Creates a 'User' from a 'Name' and @'Maybe' 'Password'@.
fromCredentials :: Name -> Maybe Password -> User
fromCredentials = User

-- | Creates a 'User' from 'Text' representations of the 'Name' and 'Password'.
-- Both the name and password's 'Text' values are unescaped when creating the 'User'.
fromTextCredentials :: Text -> Text -> Maybe User
fromTextCredentials name pw = do
  name' <- Name.fromText name
  pw' <- Password.fromText pw
  return $ User name' (Just pw')

-- | Creates a 'User' without a 'Password'.
fromNameOnly :: Name -> User
fromNameOnly = flip fromCredentials Nothing

-- | Creates a 'User' without a 'Password' using a 'Text' value to construct the 'Name'.
-- The 'Text' name is unescaped when creating the 'User'.
fromTextNameOnly :: Text -> Maybe User
fromTextNameOnly name = fromNameOnly <$> Name.fromText name

-- | Parses a @<name>[:<password>]@ string into a 'User' and unescapes both the name and password.
-- This function returns 'Nothing' if the parse fails.
fromText :: Text -> Maybe User
fromText = Parsec.parseMaybe parser "User"

-- | Creates a 'User' from a 'URI.URIAuth' for compatibility.
fromNetworkURIAuth :: URI.URIAuth -> User
fromNetworkURIAuth = undefined

-- ** Conversion

-- | Converts a 'User' to its 'Text' representation in the @[<user>[:[<password>]]]@ format.
-- The resulting name and password are escaped.
toText :: User -> Text
toText (User name maybePw) =
  case maybePw of
    Nothing -> Name.toText name
    Just pw -> Name.toText name <> ":" <> Password.toText pw

-- ** Introspection

hasNonEmptyPassword :: User -> Bool
hasNonEmptyPassword (User _ maybePw) = fmap Password.isEmpty maybePw == Just False

-- ** Helpers

-- | A parsec 'Parser' to parse a 'User'.
parser :: Parser User
parser = do
  name <- Name.parser
  pw <- (Parsec.char ':' >> Password.parser <&> Just) <|> return Nothing
  return $ fromCredentials name pw
