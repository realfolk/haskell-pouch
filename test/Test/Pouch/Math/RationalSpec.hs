{-# LANGUAGE OverloadedStrings #-}

module Test.Pouch.Math.RationalSpec
    ( spec
    ) where

import           Pouch.Math.Rational (toText, (%))
import           Test.Hspec

spec :: Spec
spec =
  toTextSpec

toTextSpec :: Spec
toTextSpec =
  describe "toText" $ do
    context "when 2 % 3" $ do
      it "returns 2/3" $ do
        toText (2 % 3) `shouldBe` "2/3"

    context "when -2 % 3" $ do
      it "returns -2/3" $ do
        toText (-2 % 3) `shouldBe` "-2/3"

    context "when 2 % -3" $ do
      it "returns -2/3" $ do
        toText (2 % (-3)) `shouldBe` "-2/3"

    context "when -2 % -3" $ do
      it "returns 2/3" $ do
        toText (-2 % (-3)) `shouldBe` "2/3"
