{-# LANGUAGE OverloadedStrings #-}

module Test.Pouch.URL.Component.QuerySpec
    ( spec
    ) where

import qualified Data.Set                as Set
import           Pouch.URL.Component.Query (Query)
import qualified Pouch.URL.Component.Query as Query
import qualified Network.Wai             as Wai
import           Test.Hspec

-- * Main

spec :: Spec
spec = do
  emptySpec
  fromListSpec
  fromNetworkQuerySpec
  fromWaiRequestSpec
  toTextSpec
  toListSpec
  toNetworkQuerySpec
  isEmptySpec
  sizeSpec
  getItemsSpec
  findItemSpec
  insertItemSpec
  deleteItemSpec
  deleteItemsSpec
  mapSpec

-- * Mock Data

mockUnescapedQuery = Query.fromList mockUnescapedList

mockEscapedQuery = Query.fromList mockEscapedList

-- Sort order matters for this list.
mockUnescapedList =
  [ ("b&z", "value")
  , ("bar", "")
  , ("foo", "/")
  , ("foo", "@")
  ]

mockUnescapedListWithDuplicates =
  [ ("b&z", "value")
  , ("b&z", "value")
  , ("bar", "")
  , ("bar", "")
  , ("bar", "")
  , ("bar", "")
  , ("foo", "/")
  , ("foo", "@")
  , ("foo", "@")
  , ("foo", "@")
  ]

mockUnescapedString = "b&z=value&bar&foo=/&foo=@"

-- Sort order matters for this list.
mockEscapedList =
  [ ("b%26z", "value")
  , ("bar", "")
  , ("foo", "%2F")
  , ("foo", "%40")
  ]

mockEscapedString = "b%26z=value&bar&foo=%2F&foo=%40"

mockEscapedNetworkQuery =
  [ ("b%26z", Just "value")
  , ("bar", Nothing)
  , ("foo", Just "%2F")
  , ("foo", Just "%40")
  ]

mockWaiRequest = Wai.defaultRequest { Wai.queryString = mockEscapedNetworkQuery }

-- * Specs

emptySpec :: Spec
emptySpec =
  describe "empty" $ do
    it "returns an empty Query" $ do
      Query.toList (Query.empty) `shouldBe` []

fromListSpec :: Spec
fromListSpec =
  describe "fromList" $ do
    context "when the argument is an empty list" $ do
      it "returns an empty Query" $ do
        Query.fromList [] `shouldBe` Query.empty
    context "when the argument is a non-empty list" $ do
      it "does not escape the items" $ do
        Query.toList (Query.fromList mockUnescapedList) `shouldBe` mockUnescapedList
      it "does not unescape the items" $ do
        Query.toList (Query.fromList mockEscapedList) `shouldBe` mockEscapedList
      context "and the list has duplicate query items" $ do
        it "removes duplicate items" $ do
          Query.toList (Query.fromList mockUnescapedListWithDuplicates) `shouldBe` mockUnescapedList

fromNetworkQuerySpec :: Spec
fromNetworkQuerySpec =
  describe "fromNetworkQuery" $ do
    context "when the argument is an empty query" $ do
      it "returns an empty Query" $ do
        Query.fromNetworkQuery [] `shouldBe` Query.empty
    context "when the argument is a non-empty query" $ do
      it "does unescape the items" $ do
        Query.fromNetworkQuery mockEscapedNetworkQuery  `shouldBe` mockUnescapedQuery

fromWaiRequestSpec :: Spec
fromWaiRequestSpec =
  describe "fromWaiRequest" $ do
    context "when the request has an empty query" $ do
      it "returns an empty Query" $ do
        Query.fromWaiRequest Wai.defaultRequest `shouldBe` Query.empty
    context "when the request has a non-empty query" $ do
      it "does unescape the items" $ do
        Query.fromWaiRequest mockWaiRequest `shouldBe` mockUnescapedQuery

toTextSpec :: Spec
toTextSpec =
  describe "toText" $ do
    context "when the query is empty" $ do
      it "returns an empty string" $ do
        Query.toText Query.empty `shouldBe` ""
    context "when the query is not empty" $ do
      it "does escape the items" $ do
        Query.toText mockUnescapedQuery `shouldBe` mockEscapedString

toListSpec :: Spec
toListSpec =
  describe "toList" $ do
    context "when the query is empty" $ do
      it "returns an empty list" $ do
        Query.toList Query.empty `shouldBe` []
    context "when the query is not empty" $ do
      it "does not escape the items" $ do
        Query.toList mockUnescapedQuery `shouldBe` mockUnescapedList
      it "does not unescape the items" $ do
        Query.toList mockEscapedQuery `shouldBe` mockEscapedList

toNetworkQuerySpec :: Spec
toNetworkQuerySpec =
  describe "toNetworkQuery" $ do
    context "when the query is empty" $ do
      it "returns an empty list" $ do
        Query.toNetworkQuery Query.empty `shouldBe` []
    context "when the query is not empty" $ do
      it "does escape the items" $ do
        Query.toNetworkQuery mockUnescapedQuery `shouldBe` mockEscapedNetworkQuery

isEmptySpec :: Spec
isEmptySpec =
  describe "isEmpty" $ do
    context "when the query is empty" $ do
      it "returns True" $ do
        Query.isEmpty Query.empty `shouldBe` True
    context "when the query is not empty" $ do
      it "returns False" $ do
        Query.isEmpty mockUnescapedQuery `shouldBe` False

sizeSpec :: Spec
sizeSpec =
  describe "size" $ do
    context "when the query is empty" $ do
      it "returns zero" $ do
        Query.size Query.empty `shouldBe` 0
    context "when the query is not empty" $ do
      it "returns the correct size" $ do
        Query.size mockUnescapedQuery `shouldBe` 4

getItemsSpec :: Spec
getItemsSpec =
  describe "getItems" $ do
    context "when the key does not exist in the query" $ do
      it "returns Nothing" $ do
        Query.getItems "foo" Query.empty `shouldBe` Nothing
    context "when the key exists in the query" $ do
      it "returns the correct values" $ do
        Query.getItems "b&z" mockUnescapedQuery `shouldBe` Just (Set.fromList ["value"])
        Query.getItems "bar" mockUnescapedQuery `shouldBe` Just (Set.fromList [""])
        Query.getItems "foo" mockUnescapedQuery `shouldBe` Just (Set.fromList ["/", "@"])

findItemSpec :: Spec
findItemSpec =
  describe "findItem" $ do
    context "when predicate is never satisfied" $ do
      let result = Query.findItem (const False) mockUnescapedQuery
      it "returns a tuple whose first element is Nothing" $ do
        fst result `shouldBe` Nothing
      it "returns a tuple whose second element is the original query" $ do
        snd result `shouldBe` mockUnescapedQuery
    context "when predicate is satisfied" $ do
      context "and the predicate matches a single value for a single key" $ do
        let result = Query.findItem (\(k, v) -> k == "bar" && v == "") mockUnescapedQuery
        it "returns a tuple whose first element is the first matching item" $ do
          fst result `shouldBe` Just ("bar", "")
        it "returns a tuple whose second element is the original query that excludes the found item" $ do
          -- For some reason, equality fails when comparing two
          -- identical Query values, so convert to list to ensure the test
          -- passes when valid. Might be a compiler error.
          Query.toList (snd result) `shouldBe` [ ("b&z", "value")
                                               , ("foo", "/")
                                               , ("foo", "@")
                                               ]
      context "and the predicate matches multiple values for a single key" $ do
        let result = Query.findItem (\(k, _) -> k == "foo") mockUnescapedQuery
        it "returns a tuple whose first element is the first matching item" $ do
          fst result `shouldBe` Just ("foo", "/")
        it "returns a tuple whose second element is the original query that excludes the found item" $ do
          snd result `shouldBe` Query.fromList
                                  [ ("b&z", "value")
                                  , ("bar", "")
                                  , ("foo", "@")
                                  ]

insertItemSpec :: Spec
insertItemSpec =
  describe "insertItem" $ do
    context "when the item doesn't exist" $ do
      context "and the key doesn't have other values" $ do
        it "successfully inserts the item" $ do
          Query.toList (Query.insertItem ("bop", "123") mockUnescapedQuery) `shouldBe` [ ("b&z", "value")
                                                                                       , ("bar", "")
                                                                                       , ("bop", "123")
                                                                                       , ("foo", "/")
                                                                                       , ("foo", "@")
                                                                                       ]
      context "and the key does have other values" $ do
        it "successfully inserts the item" $ do
          Query.toList (Query.insertItem ("foo", "123") mockUnescapedQuery) `shouldBe` [ ("b&z", "value")
                                                                                       , ("bar", "")
                                                                                       , ("foo", "/")
                                                                                       , ("foo", "123")
                                                                                       , ("foo", "@")
                                                                                       ]
    context "when the item already exists" $ do
      it "does not change the query" $ do
        Query.insertItem ("foo", "/") mockUnescapedQuery `shouldBe` mockUnescapedQuery

deleteItemSpec :: Spec
deleteItemSpec =
  describe "deleteItem" $ do
    context "when the item exists" $ do
      context "and the key doesn't have other values" $ do
        let result = Query.deleteItem ("bar", "") mockUnescapedQuery
        it "successfully deletes the item" $ do
          result `shouldBe` Query.fromList
                              [ ("b&z", "value")
                              , ("foo", "/")
                              , ("foo", "@")
                              ]
        it "removes the empty set from the query" $ do
          Query.getItems "bar" result `shouldBe` Nothing
      context "and the key does have other values" $ do
        it "successfully deletes the item" $ do
          Query.deleteItem ("foo", "/") mockUnescapedQuery `shouldBe` Query.fromList
                                                                        [ ("b&z", "value")
                                                                        , ("bar", "")
                                                                        , ("foo", "@")
                                                                        ]
    context "when the item doesn't exist" $ do
      it "does not change the query" $ do
        Query.deleteItem ("foo", "123") mockUnescapedQuery `shouldBe` mockUnescapedQuery

deleteItemsSpec :: Spec
deleteItemsSpec =
  describe "deleteItems" $ do
    context "when the key exists" $ do
      it "successfully deletes the item" $ do
        Query.deleteItems "foo" mockUnescapedQuery `shouldBe` Query.fromList
                                                                [ ("b&z", "value")
                                                                , ("bar", "")
                                                                ]
    context "when the key doesn't exist" $ do
      it "does not change the query" $ do
        Query.deleteItems "bop"  mockUnescapedQuery `shouldBe` mockUnescapedQuery

mapSpec :: Spec
mapSpec =
  describe "map" $ do
    it "correctly maps each query item" $ do
      Query.map (\(k, v) -> (k <> k, v <> v)) mockUnescapedQuery `shouldBe` Query.fromList
                                                                              [ ("b&zb&z", "valuevalue")
                                                                              , ("barbar", "")
                                                                              , ("foofoo", "//")
                                                                              , ("foofoo", "@@")
                                                                              ]
