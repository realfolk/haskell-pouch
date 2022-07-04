{-# LANGUAGE UndecidableInstances #-}

module Lib.Binary
    ( GetBinary (..)
    , PutBinary (..)
    , decodeMaybe
    , decodeStrict
    , decodeStrictMaybe
    , decodeStrictOrFail
    , encodeStrict
    ) where

import           Data.Bifunctor       (bimap)
import           Data.Binary          (Binary)
import qualified Data.Binary          as Binary
import           Data.Binary.Get      (ByteOffset)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Lib.Either           as Either
import qualified Lib.Tuple            as Tuple

-- * GetBinary Typeclass

class GetBinary a where
  getBinary :: Binary.Get a

-- * PutBinary Typeclass

class PutBinary a where
  putBinary :: a -> Binary.Put

-- * Helpers

-- ** Strict Encoding/Decoding

encodeStrict :: Binary a => a -> ByteString
encodeStrict = LazyByteString.toStrict . Binary.encode

decodeStrict :: Binary a => ByteString -> a
decodeStrict = Binary.decode . LazyByteString.fromStrict

decodeStrictOrFail :: Binary a => ByteString -> Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)
decodeStrictOrFail = bimap toStrict toStrict . Binary.decodeOrFail . LazyByteString.fromStrict
  where
    toStrict (a, b, c) = (LazyByteString.toStrict a, b, c)

decodeStrictMaybe :: Binary a => ByteString -> Maybe a
decodeStrictMaybe = fmap Tuple.third . Either.toMaybe . decodeStrictOrFail

-- ** Lazy Encoding/Decoding

decodeMaybe :: Binary a => LazyByteString.ByteString -> Maybe a
decodeMaybe = fmap Tuple.third . Either.toMaybe . Binary.decodeOrFail
