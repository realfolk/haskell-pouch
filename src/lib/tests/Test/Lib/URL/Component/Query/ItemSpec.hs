{-# LANGUAGE OverloadedStrings #-}

module Test.Lib.URL.Component.Query.ItemSpec
    ( spec
    ) where

import           Lib.URL.Component.Query.Item (Item)
import qualified Lib.URL.Component.Query.Item as Item
import qualified Network.Wai                  as Wai
import           Test.Hspec

-- * Main

spec :: Spec
spec = do
  fromNetworkQueryItemSpec
  toNetworkQueryItemSpec
  unescapeSpec
  escapeSpec

-- * Mock Data

mockUnescapedItem = ("key&/", "value@&")

mockEscapedItem = ("key%26%2F", "value%40%26")

-- * Specs

fromNetworkQueryItemSpec :: Spec
fromNetworkQueryItemSpec =
  describe "fromNetworkQueryItem" $ do
    it "does not unescape the key or value" $ do
      Item.fromNetworkQueryItem ("key%26", Just "value%40") `shouldBe` ("key%26", "value%40")
    it "does not escape the key or value" $ do
      Item.fromNetworkQueryItem ("key&", Just "value@") `shouldBe` ("key&", "value@")
    context "when the value is Nothing" $ do
      it "returns an Item with an empty value" $ do
        Item.fromNetworkQueryItem ("key", Nothing) `shouldBe` ("key", "")
    context "when the value is Just" $ do
      it "returns an Item with the correct value" $ do
        Item.fromNetworkQueryItem ("key", Just "value") `shouldBe` ("key", "value")

toNetworkQueryItemSpec :: Spec
toNetworkQueryItemSpec =
  describe "toNetworkQueryItem" $ do
    it "does not unescape the key or value" $ do
      Item.toNetworkQueryItem ("key%26", "value%40") `shouldBe` ("key%26", Just "value%40")
    it "does not escape the key or value" $ do
      Item.toNetworkQueryItem ("key&", "value@") `shouldBe` ("key&", Just "value@")
    context "when the value is empty" $ do
      it "returns a network query item with a Nothing value" $ do
        Item.toNetworkQueryItem ("key", "") `shouldBe` ("key", Nothing)
    context "when the value is not empty" $ do
      it "returns a network query item with the correct value" $ do
        Item.toNetworkQueryItem ("key", "value") `shouldBe` ("key", Just "value")

unescapeSpec :: Spec
unescapeSpec =
  describe "unescape" $ do
    it "correctly unescapes the key and value" $ do
      Item.unescape mockEscapedItem `shouldBe` mockUnescapedItem

escapeSpec :: Spec
escapeSpec =
  describe "escape" $ do
  it "correctly escapes the key and value" $ do
    Item.escape mockUnescapedItem `shouldBe` mockEscapedItem
