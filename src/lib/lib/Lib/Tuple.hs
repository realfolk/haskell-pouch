module Lib.Tuple
    ( third
    ) where

third :: (a, b, c) -> c
third (_, _, c) = c
