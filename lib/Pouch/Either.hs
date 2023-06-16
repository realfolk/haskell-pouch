module Pouch.Either
    ( toMaybe
    ) where

toMaybe :: Either a b -> Maybe b
toMaybe = either (const Nothing) Just
