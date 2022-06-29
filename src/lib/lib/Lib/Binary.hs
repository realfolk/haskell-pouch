module Lib.Binary
    ( decodeStrict
    , decodeStrictOrFail
    , encodeStrict
    ) where

import           Data.Bifunctor       (bimap)
import           Data.Binary          (Binary)
import qualified Data.Binary          as Binary
import           Data.Binary.Get      (ByteOffset)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

encodeStrict :: Binary a => a -> ByteString
encodeStrict = LazyByteString.toStrict . Binary.encode

decodeStrict :: Binary a => ByteString -> a
decodeStrict = Binary.decode . LazyByteString.fromStrict

decodeStrictOrFail :: Binary a => ByteString -> Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)
decodeStrictOrFail = bimap toStrict toStrict . Binary.decodeOrFail . LazyByteString.fromStrict
  where
    toStrict (a, b, c) = (LazyByteString.toStrict a, b, c)
