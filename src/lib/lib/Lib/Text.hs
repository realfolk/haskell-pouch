module Lib.Text
    ( padLeft
    , padRight
    , tshow
    ) where

import qualified Data.Text as T

padLeft :: Int -> Char -> T.Text -> T.Text
padLeft n c t =
  if required > 0
     then T.pack (replicate required c) <> t
     else t
  where
    required = n - T.length t

padRight :: Int -> Char -> T.Text -> T.Text
padRight n c t =
  if required > 0
     then t <> T.pack (replicate required c)
     else t
  where
    required = n - T.length t

tshow :: Show a => a -> T.Text
tshow = T.pack . show
