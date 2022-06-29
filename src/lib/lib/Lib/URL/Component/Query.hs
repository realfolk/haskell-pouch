{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module: Lib.URL.Component.Query
-- Description: A library for interacting with URL queries.
-- Copyright: (c) Real Folk Inc. 2022
-- Maintainer: admin@realfolk.com
-- Stability: experimental
-- Portability: POSIX
--
-- This library includes a type, 'Query', for representing URL query components, and associated functions.
-- While simple queries are generally easy to reason about, there are certain situations that require
-- implementations like this to have specific opinions that may not be standard across all query string libraries.
--
-- For example, if a query string has the same key multiple times with different values (@foo=bar&foo=baz@), we must
-- decide whether the key (@foo@) should only have a single value or multiple values. If we choose the former, then we
-- need to define logic to select the value from the query string (@bar@ or @baz@?). Otherwise, we can represent
-- both values as a list (@["bar", "baz"]@), however this creates a possible edge case in our library where a key may
-- have no values (@[]@), which may not be serializable to a valid query string.
--
-- This example illustrates the challenges we face when implementing a library like this. Supporting query strings
-- requires opinionated choices that reflect varying degrees of compromise.

module Lib.URL.Component.Query
    ( Query
    , deleteItem
    , deleteItems
    , empty
    , findItem
    , fromList
    , fromNetworkQuery
    , fromText
    , fromWaiRequest
    , getItems
    , insertItem
    , isEmpty
    , map
    , parser
    , size
    , toList
    , toNetworkQuery
    , toText
    ) where

import           Data.Bifunctor               (bimap, first, second)
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Binary
import           Data.Functor                 ((<&>))
import qualified Data.List                    as List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified GHC.Exts
import qualified Lib.Parsec                   as Parsec
import           Lib.URL.Component.Query.Item (Item)
import qualified Lib.URL.Component.Query.Item as Item
import qualified Network.HTTP.Types.URI       as URI
import qualified Network.Wai                  as Wai
import           Prelude                      hiding (map)
import qualified Text.Parsec                  as Parsec
import           Text.Parsec.Text             (Parser)

-- * Query Type

-- | The representation of a query. Each key ('Text') is mapped to a 'Set Text' of values.
-- The 'Set' ensures values are deduplicated on a per-key basis. Values cannot exist without keys.
-- A key should never be mapped to an empty 'Set' (this should be an impossible state).
-- Keys present in a query string without a value are assumed to have the value @"" :: 'Text'@.
-- For example, in the query @foo=bar&bop&baz=123@, the @bop@ key is assumed to have a value of @""@.
newtype Query
  = Query (Map Text (Set Text))
  deriving (Eq, Ord)

instance Show Query where
  show = Text.unpack . toText

instance GHC.Exts.IsList Query where
  type Item Query = Item.Item
  fromList = fromList
  toList = toList

instance Binary Query where
  put (Query kvs) = do
    Binary.putWord8 0
    Binary.put kvs
  get = do
    id' <- Binary.getWord8
    case id' of
      0 -> Query <$> Binary.get
      _ -> fail "Invalid ID"

-- * Constructors

-- | An empty 'Query' with no keys.
empty :: Query
empty = Query Map.empty

-- | Creates a 'Query' from a list of 'Item's. This function does
-- not perform any escaping or unescaping.
fromList :: [Item] -> Query
fromList = foldr insertItem empty

-- | Creates a 'Query' from a 'URI.Query' for compatibility. This function unescapes the
-- provided argument, so the resulting 'Query' is unescaped.
fromNetworkQuery :: URI.Query -> Query
fromNetworkQuery = fromList . List.map Item.fromNetworkQueryItem

-- | Creates a 'Query' from the query in a 'Wai.Request'. This function uses
-- 'fromNetworkQuery' internally, so the resulting 'Query' is unescaped.
fromWaiRequest :: Wai.Request -> Query
fromWaiRequest = fromNetworkQuery . Wai.queryString

-- | Parses a query string into a 'Query' and unescapes all keys and values. The supplied
-- 'Text' can optionally include a leading @?@ character. This function returns 'Nothing'
-- if the parse fails.
fromText :: Text -> Maybe Query
fromText = Parsec.parseMaybe parser "Query"

-- * Conversion

-- | Converts a 'Query' to its query string representation as 'Text'. This function
-- escapes all keys and values.
toText :: Query -> Text
toText = foldr f "" . toList
  where
    f item acc = Item.toText item <> ampersand acc
    ampersand acc = if acc == "" then "" else "&" <> acc

-- | Converts a 'Query' into a list of 'Item's. This function does not perform
-- any escaping or unescaping.
toList :: Query -> [Item]
toList (Query query) = Map.foldrWithKey createList [] query
  where
    createList k v acc = List.map (k,) (Set.toList v) <> acc

-- | Converts a 'Query' into a 'URI.Query' for compatibility. This function escapes
-- all keys and values.
toNetworkQuery :: Query -> URI.Query
toNetworkQuery = List.map Item.toNetworkQueryItem . toList

-- * Introspection

-- | Returns 'True' if the supplied 'Query' is empty, and 'False' if it is not empty.
isEmpty :: Query -> Bool
isEmpty (Query query) = Map.null query

-- | Returns the number of key-value pairs in a 'Query'. This function returns a 'Word'
-- instead of an 'Int' because the size of a 'Query' cannot be negative.
size :: Query -> Word
size (Query query) = foldr count 0 query
  where
    count v acc =
      acc + fromIntegral (Set.size v)

-- | Gets the 'Set' of values associated with a 'Text' key in a 'Query'.
-- Returns 'Nothing' if the key does not exist.
getItems :: Text -> Query -> Maybe (Set Text)
getItems k (Query query) = Map.lookup k query

-- | Finds an 'Item' in a 'Query' that matches the supplied predicate.
-- This function returns 'Nothing' for the match if it cannot be found.
-- This function also returns the original 'Query' with the match excluded, if any.
-- If multiple matches are found, only the first matching 'Item' is returned, and all
-- other matches are included in the return 'Query'.
findItem :: (Item -> Bool) -> Query -> (Maybe Item, Query)
findItem predicate (Query query) =
  second Query $ Map.foldrWithKey search (Nothing, Map.empty) query
  where
    search k v (found, nonMatches) =
      case found of
        Just _ -> (found, Map.insert k v nonMatches)
        Nothing ->
          let (found', v') = searchWithinPair k v
           in (found', Map.insert k v' nonMatches)
    searchWithinPair k v =
      let (matches, nonMatches') = Set.partition (predicate . (k,)) v
       in case Set.minView matches of
            Nothing -> (Nothing, nonMatches')
            Just (match, otherMatches) -> (Just (k, match), otherMatches <> nonMatches')

-- * Modification

-- | Inserts an 'Item' into a 'Query'.
insertItem :: Item -> Query -> Query
insertItem (k, v) (Query query) = Query (Map.alter (replaceOrInsert v) k query)
  where
    replaceOrInsert newValue maybeValue =
      case maybeValue of
        Nothing             -> Just $ Set.singleton newValue
        Just existingValues -> Just $ Set.insert newValue existingValues

-- | Deletes an 'Item' from a 'Query'.
deleteItem :: Item -> Query -> Query
deleteItem (k, v) (Query query) = Query (Map.update (deleteOrKeep . Set.delete v) k query)
  where
    deleteOrKeep newSet =
      -- This function is responsible for ensuring the 'Set' values
      -- are not retained if they are empty after 'Item' deletion.
      if Set.null newSet
         then Nothing
         else Just newSet

-- | Deletes all 'Set Text' values associated with a 'Text' key in a 'Query'.
deleteItems :: Text -> Query -> Query
deleteItems k (Query query) = Query (Map.delete k query)

-- * Helpers

-- | Maps over a the 'Item's in a 'Query'.
map :: (Item -> Item) -> Query -> Query
map f = fromList . List.map f . toList

-- | A parsec 'Parser' to parse a 'Query' in the format @key=value[&key=value...]@.
-- This function uses 'Item.parser' to parse each key-value pair.
-- This parser unescapes all keys and values.
parser :: Parser Query
parser = Parsec.sepBy Item.parser ampersand <&> fromList
  where
    ampersand = Parsec.char '&'
