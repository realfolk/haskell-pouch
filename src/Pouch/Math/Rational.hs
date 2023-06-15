{-# LANGUAGE OverloadedStrings #-}

module Pouch.Math.Rational
    ( Rational
    , denominator
    , numerator
    , toText
    , (%)
    ) where

import           Data.Ratio (denominator, numerator, (%))
import qualified Data.Text  as T
import           Pouch.Text   (tshow)

toText :: Rational -> T.Text
toText r = tshow n <> "/" <> tshow d
  where
    n = numerator r
    d = denominator r
