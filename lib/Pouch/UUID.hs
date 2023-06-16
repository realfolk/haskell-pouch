module Pouch.UUID
    ( UUID
    , UUID.nextRandom
    , encode
    , decode
    , encodeStrict
    , decodeStrict
    , encodeText
    , decodeText
    , encodeTextStrict
    , decodeTextStrict
    , encodeBase64
    , decodeBase64
    , encodeBase64Strict
    , decodeBase64Strict
    , encodeBase64Text
    , decodeBase64Text
    , encodeBase64TextStrict
    , decodeBase64TextStrict
    , encodeBase58
    , decodeBase58
    , encodeBase58Strict
    , decodeBase58Strict
    , encodeBase58Text
    , decodeBase58Text
    , encodeBase58TextStrict
    , decodeBase58TextStrict
    )
    where

import Data.UUID (UUID)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString         as BS
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import qualified Pouch.Base58              as B58
import qualified Pouch.Base64              as B64
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE

encode :: UUID -> LBS.ByteString
encode = UUID.toByteString

decode :: LBS.ByteString -> Maybe UUID
decode = UUID.fromByteString

encodeStrict :: UUID -> BS.ByteString
encodeStrict = LBS.toStrict . encode

decodeStrict :: BS.ByteString -> Maybe UUID
decodeStrict = decode . LBS.fromStrict

encodeText :: UUID -> LT.Text
encodeText  = LT.fromStrict . encodeTextStrict

decodeText :: LT.Text -> Maybe UUID
decodeText = decodeTextStrict . LT.toStrict

encodeTextStrict :: UUID -> T.Text
encodeTextStrict = UUID.toText

decodeTextStrict :: T.Text -> Maybe UUID
decodeTextStrict = UUID.fromText

encodeBase58 :: UUID -> LBS.ByteString
encodeBase58 = B58.encode . encode

decodeBase58 :: LBS.ByteString -> Maybe UUID
decodeBase58 bs = B58.decode bs >>= decode

encodeBase58Strict :: UUID -> BS.ByteString
encodeBase58Strict = B58.encodeStrict . encodeStrict

decodeBase58Strict :: BS.ByteString -> Maybe UUID
decodeBase58Strict bs = B58.decodeStrict bs >>= decodeStrict

encodeBase58Text :: UUID -> LT.Text
encodeBase58Text = LTE.decodeUtf8 . encodeBase58

decodeBase58Text :: LT.Text -> Maybe UUID
decodeBase58Text = decodeBase58 . LTE.encodeUtf8

encodeBase58TextStrict :: UUID -> T.Text
encodeBase58TextStrict = TE.decodeUtf8 . encodeBase58Strict

decodeBase58TextStrict :: T.Text -> Maybe UUID
decodeBase58TextStrict = decodeBase58Strict . TE.encodeUtf8

encodeBase64 :: UUID -> LBS.ByteString
encodeBase64 = B64.encode . encode

decodeBase64 :: LBS.ByteString -> Maybe UUID
decodeBase64 bs = B64.decode bs >>= decode

encodeBase64Strict :: UUID -> BS.ByteString
encodeBase64Strict = B64.encodeStrict . encodeStrict

decodeBase64Strict :: BS.ByteString -> Maybe UUID
decodeBase64Strict bs = B64.decodeStrict bs >>= decodeStrict

encodeBase64Text :: UUID -> LT.Text
encodeBase64Text = LTE.decodeUtf8 . encodeBase64

decodeBase64Text :: LT.Text -> Maybe UUID
decodeBase64Text = decodeBase64 . LTE.encodeUtf8

encodeBase64TextStrict :: UUID -> T.Text
encodeBase64TextStrict = TE.decodeUtf8 . encodeBase64Strict

decodeBase64TextStrict :: T.Text -> Maybe UUID
decodeBase64TextStrict = decodeBase64Strict . TE.encodeUtf8
