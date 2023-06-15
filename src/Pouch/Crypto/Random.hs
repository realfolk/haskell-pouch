module Pouch.Crypto.Random
    ( Generator
    , generate
    , generateStrict
    , newGenerator
    , newTestGenerator
    , with
    , withStrict
    ) where

import qualified Crypto.Random        as Random
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Word            (Word64)

type Generator = Random.ChaChaDRG

newGenerator :: IO Generator
newGenerator = Random.drgNew

newTestGenerator :: (Word64, Word64, Word64, Word64, Word64) -> Generator
newTestGenerator = Random.drgNewTest

generate :: Int -> Generator -> (LBS.ByteString, Generator)
generate n gen = first LBS.fromStrict $ generateStrict n gen

generateStrict :: Int -> Generator -> (BS.ByteString, Generator)
generateStrict = Random.randomBytesGenerate

with :: Int -> Generator -> (LBS.ByteString -> a) -> (a, Generator)
with n gen f = withStrict n gen (f . LBS.fromStrict)

withStrict :: Int -> Generator -> (BS.ByteString -> a) -> (a, Generator)
withStrict n gen = Random.withRandomBytes gen n
