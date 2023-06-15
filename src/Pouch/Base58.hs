module Pouch.Base58
    ( encode
    , decode
    , encodeStrict
    , decodeStrict
    , encodeText
    , decodeText
    , encodeTextStrict
    , decodeTextStrict
    )
    where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base58  as B58
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE

encode :: LBS.ByteString -> LBS.ByteString
encode = LBS.fromStrict . encodeStrict . LBS.toStrict

decode :: LBS.ByteString -> Maybe LBS.ByteString
decode bs = LBS.fromStrict <$> decodeStrict (LBS.toStrict bs)

encodeStrict :: BS.ByteString -> BS.ByteString
encodeStrict = B58.encodeBase58 B58.bitcoinAlphabet

decodeStrict :: BS.ByteString -> Maybe BS.ByteString
decodeStrict = B58.decodeBase58 B58.bitcoinAlphabet

encodeText :: LBS.ByteString -> LT.Text
encodeText = LTE.decodeUtf8 . encode

decodeText :: LBS.ByteString -> Maybe LT.Text
decodeText bs = LTE.decodeUtf8 <$> decode bs

encodeTextStrict :: BS.ByteString -> T.Text
encodeTextStrict = TE.decodeUtf8 . encodeStrict

decodeTextStrict :: BS.ByteString -> Maybe T.Text
decodeTextStrict bs = TE.decodeUtf8 <$> decodeStrict bs
