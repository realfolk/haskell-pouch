module Pouch.Crypto.Hash
    ( Digest
    , decode
    , decodeStrict
    , encode
    , encodeStrict
    ) where

import qualified Crypto.Hash          as Hash
import qualified Data.ByteArray       as BA
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

type Digest a = Hash.Digest a

encode :: Digest a -> LBS.ByteString
encode = LBS.fromStrict . encodeStrict

decode :: Hash.HashAlgorithm a => LBS.ByteString -> Maybe (Digest a)
decode = decodeStrict . LBS.toStrict

encodeStrict :: Digest a -> BS.ByteString
encodeStrict = BA.convert

decodeStrict :: Hash.HashAlgorithm a => BS.ByteString -> Maybe (Digest a)
decodeStrict = Hash.digestFromByteString
