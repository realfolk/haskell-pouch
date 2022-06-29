module Lib.Crypto.Password
    ( authorize
    , authorizeStrict
    , hash
    , hashStrict
    , validate
    , validateStrict
    ) where


import           Control.Monad.Except    (MonadError (throwError))
import qualified Crypto.KDF.BCrypt       as BCrypt
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE

-- | The default BCrypt cost value.
costValue = 8 :: Int

hash :: LT.Text -> IO LBS.ByteString
hash password = fmap LBS.fromStrict $ hashStrict $ LT.toStrict password

hashStrict :: T.Text -> IO BS.ByteString
hashStrict = BCrypt.hashPassword costValue . TE.encodeUtf8

validate :: LT.Text -> LBS.ByteString -> Bool
validate password hash' = validateStrict (LT.toStrict password) (LBS.toStrict hash')

validateStrict :: T.Text -> BS.ByteString -> Bool
validateStrict password = BCrypt.validatePassword (TE.encodeUtf8 password)

authorize :: MonadError e m => e -> LT.Text -> LBS.ByteString -> m a -> m a
authorize error' password hash' = authorizeStrict error' (LT.toStrict password) (LBS.toStrict hash')

authorizeStrict :: MonadError e m => e -> T.Text -> BS.ByteString -> m a -> m a
authorizeStrict error' password hash' action =
  if validateStrict password hash'
     then action
     else throwError error'
