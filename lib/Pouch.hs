{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Pouch
-- Description: A utility library for processing 'T.Text' and 'ByteString' values.
-- Copyright: (c) Real Folk Inc. 2022
-- Maintainer: admin@realfolk.com
-- Stability: experimental
-- Portability: POSIX
--
-- This library includes utility functions for prefixing and processing base64 'ByteString' values.
module Pouch where

import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Word                     as W
import           Numeric.Natural               (Natural)

-- | Simple inline function for conditional logic.
iff :: Bool -> a -> a -> a
iff condition a b =
  if condition
    then a
    else b

-- | An alternative to 'iff' that returns 'mempty' if the @condition@ is 'False'.
iff' :: Monoid a => Bool -> a -> a
iff' condition a = iff condition a mempty

-- | Map over a list with each element's index, beginning at @0@.
mapWithIndex :: (Num i, Enum i) => (i -> a -> b) -> [a] -> [b]
mapWithIndex f as = map (uncurry f) $ zip [0 ..] as

-- | The prefix for a base64 string of text.
base64Prefix :: T.Text
{-# INLINE base64Prefix #-}
base64Prefix = "base64:"

-- | Encodes a 'ByteString' to base64 'T.Text'.
encodeBase64 :: BS.ByteString -> T.Text
{-# INLINE encodeBase64 #-}
encodeBase64 = T.append base64Prefix . TE.decodeUtf8 . B64.encode

-- | Decodes a base64 'T.Text' value to either an error message or a 'ByteString' value.
decodeBase64 :: T.Text -> Either String BS.ByteString
{-# INLINE decodeBase64 #-}
decodeBase64 t
  | T.take (T.length base64Prefix) t == base64Prefix =
      B64.decode (TE.encodeUtf8 base64String)
  | otherwise = Left "Not a Base64 String"
  where
    base64String = T.drop (T.length base64Prefix) t

-- | Convert an 'Either' to a 'Bool'.
eitherToBool :: Either a b -> Bool
eitherToBool (Right _) = True
eitherToBool (Left _)  = False

-- | Safely convert an 'Integral' value to another bounded 'Integral' value if the first value is within the bounds of the second value.
fromIntegralWithinBounds ::
  forall a b.
  (Integral a, Integral b, Num b, Bounded b) =>
  a ->
  Maybe b
fromIntegralWithinBounds a
  | a >= (fromIntegral (minBound :: b)) && a <= (fromIntegral (maxBound :: b)) =
      Just $ fromIntegral a
  | otherwise = Nothing

-- | Safely convert an 'Integral' value to a 'Natural'.
integralToNatural :: Integral a => a -> Maybe Natural
integralToNatural i
  | i >= 0 = Just $ fromIntegral i
  | otherwise = Nothing

-- | Convert a hexadecimal number into a 'W.Word8' if it is within bounds of a byte.
hexToByte :: String -> Maybe W.Word8
hexToByte s = hexToDec s >>= fromIntegralWithinBounds

-- | Convert a hexadecimal number to a 'Natural'.
hexToDec :: String -> Maybe Natural
hexToDec s = fst <$> loop s
  where
    loop s =
      case s of
        [] -> return (0, 0)
        c : cs -> do
          (n, exp) <- loop cs
          m <- hexDigitToDec c exp
          return (n + (fromIntegral m), exp + 1)

-- | Convert a hexadecimal digit to a 'Natural' number.
hexDigitToDec :: Char -> W.Word64 -> Maybe W.Word8
hexDigitToDec c exp =
  (*) (16 ^ exp)
    <$> case c of
      '0' -> Just 0
      '1' -> Just 1
      '2' -> Just 2
      '3' -> Just 3
      '4' -> Just 4
      '5' -> Just 5
      '6' -> Just 6
      '7' -> Just 7
      '8' -> Just 8
      '9' -> Just 9
      'a' -> Just 10
      'b' -> Just 11
      'c' -> Just 12
      'd' -> Just 13
      'e' -> Just 14
      'f' -> Just 15
      _   -> Nothing
