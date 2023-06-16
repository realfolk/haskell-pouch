{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Pouch.URL.HTTP
    ( Host
    , Path
    , Port
    , Query
    , URL
    , User
    , fragment
    , fromNetworkURI
    , fromText
    , host
    , parser
    , path
    , port
    , query
    , secure
    , toNetworkURI
    , toText
    , user
    ) where

import           Control.Applicative          ((<|>))
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Binary
import           Data.Functor                 (($>), (<&>))
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Word                    (Word16)
import           Lens.Micro                   (Lens')
import qualified Lens.Micro                   as Lens
import           Network.URI                  (URI)
import qualified Network.URI                  as URI
import qualified Pouch.Parsec                 as Parsec
import           Pouch.URL.Component.Fragment (Fragment)
import qualified Pouch.URL.Component.Fragment as Fragment
import           Pouch.URL.Component.Host     (Host)
import qualified Pouch.URL.Component.Host     as Host
import           Pouch.URL.Component.Path     (Path)
import qualified Pouch.URL.Component.Path     as Path
import           Pouch.URL.Component.Port     (Port)
import qualified Pouch.URL.Component.Port     as Port
import           Pouch.URL.Component.Query    (Query)
import qualified Pouch.URL.Component.Query    as Query
import           Pouch.URL.Component.User     (User)
import qualified Pouch.URL.Component.User     as User
import qualified Text.Parsec                  as Parsec
import qualified Text.Parsec.Char             as Parsec
import           Text.Parsec.Text             (Parser)

-- * URL

data URL
  = URL
      { _secure   :: Bool
      , _user     :: Maybe User
      , _host     :: Host
      , _port     :: Maybe Port
      , _path     :: Path
      , _query    :: Query
      , _fragment :: Fragment
      }
  deriving (Eq)

instance Show URL where
  show = Text.unpack . toText

instance Binary URL where
  put (URL {..})= do
    Binary.putWord8 0
    Binary.put _secure
    Binary.put _user
    Binary.put _host
    Binary.put _port
    Binary.put _path
    Binary.put _query
    Binary.put _fragment
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> URL <$> Binary.get
               <*> Binary.get
               <*> Binary.get
               <*> Binary.get
               <*> Binary.get
               <*> Binary.get
               <*> Binary.get
      _ -> fail "Invalid ID"

-- ** Constructors

fromText :: Text -> Maybe URL
fromText = Parsec.parseMaybe parser "HTTP URL"

fromNetworkURI :: URI -> Maybe URL
fromNetworkURI = undefined

-- ** Conversion

toText :: URL -> Text
toText (URL {..}) =
  Text.concat
    [ if _secure then "https" else "http"
    , "://"
    , user'
    , Host.toText True _host
    , port'
    , Path.toText _path
    , query'
    , fragment'
    ]
    where
      user' = fromMaybe "" $ do
        u <- _user
        return $ User.toText u <> "@"
      port' = fromMaybe "" $ do
        p <- _port
        return $ ":" <> Port.toText p
      query' = if Query.isEmpty _query then "" else "?" <> Query.toText _query
      fragment' = if Fragment.isEmpty _fragment then "" else "#" <> Fragment.toText _fragment

toNetworkURI :: URL -> URI
toNetworkURI = undefined

-- ** Lenses

secure :: Lens' URL Bool
secure = Lens.lens _secure (\url a -> url { _secure = a })

user :: Lens' URL (Maybe User)
user = Lens.lens _user (\url a -> url { _user = a })

host :: Lens' URL Host
host = Lens.lens _host (\url a -> url { _host = a })

port :: Lens' URL (Maybe Port)
port = Lens.lens _port (\url a -> url { _port = a })

path :: Lens' URL Path
path = Lens.lens _path (\url a -> url { _path = a })

query :: Lens' URL Query
query = Lens.lens _query (\url a -> url { _query = a })

fragment :: Lens' URL Fragment
fragment = Lens.lens _fragment (\url a -> url { _fragment = a })

-- ** Helpers

parser :: Parser URL
parser = do
  secure' <- secureParser
  user' <- Parsec.try (User.parser <* Parsec.char '@' <&> Just) <|> return Nothing
  host' <- Host.parser True
  -- No 'Parsec.try' needed for parsing the 'Port', since @:@ does not follow.
  port' <- (Parsec.char ':' >> Port.parser <&> Just) <|> return Nothing
  path' <- Path.parser
  -- No 'Parsec.try' needed for parsing the 'Query' , since @?@ does not follow.
  query' <- (Parsec.char '?' >> Query.parser) <|> return Query.empty
  -- No 'Parsec.try' needed for parsing the 'Fragment' , since @#@ does not follow.
  fragment' <- (Parsec.char '#' >> Fragment.parser) <|> return Fragment.empty
  return $ URL secure' user' host' port' path' query' fragment'
  where
    secureParser = do
      Parsec.string "http"
      (Parsec.string "://" $> False) <|> (Parsec.string "s://" $> True)
