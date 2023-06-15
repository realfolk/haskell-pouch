module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text             as Text
import qualified Pouch.Crypto.Password as Password
import qualified System.Environment    as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [] -> print "You must provide a password to encrypt."
    [password] -> do
      hash <- Password.hashStrict $ Text.pack password
      ByteString.putStrLn hash
    _ ->
      print "Too many arguments provided. Please provide a single password to encrypt."
