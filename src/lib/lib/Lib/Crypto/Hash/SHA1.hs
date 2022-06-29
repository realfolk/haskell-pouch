module Lib.Crypto.Hash.SHA1
    ( SHA1
    , hash
    , hashStrict
    , random
    , size
    ) where

import qualified Crypto.Hash          as HashExt
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Lib.Base58           as B58
import qualified Lib.Base64           as B64
import qualified Lib.Crypto.Hash      as Hash
import qualified Lib.Crypto.Random    as Random

type SHA1 = HashExt.SHA1

-- | The byte-length of SHA1 hashes.
size :: Int
size = HashExt.hashDigestSize HashExt.SHA1

random :: Random.Generator -> (Hash.Digest SHA1, Random.Generator)
random gen = Random.withStrict size gen hashStrict

hash :: LBS.ByteString -> Hash.Digest SHA1
hash = HashExt.hashlazy

hashStrict :: BS.ByteString -> Hash.Digest SHA1
hashStrict = HashExt.hash
