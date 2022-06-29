{-# LANGUAGE OverloadedStrings #-}

module Lib.Time.Month
    ( Month (..)
    , fromInteger
    , toInteger
    , toShortText
    , toText
    ) where

import qualified Data.Text as T
import           Prelude   hiding (fromInteger, toInteger)

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving
    ( Enum
    , Eq
    , Show
    )

toText :: Month -> T.Text
toText month =
  case month of
    January ->
      "January"

    February ->
      "February"

    March ->
      "March"

    April ->
      "April"

    May ->
      "May"

    June ->
      "June"

    July ->
      "July"

    August ->
      "August"

    September ->
      "September"

    October ->
      "October"

    November ->
      "November"

    December ->
      "December"


toShortText :: Month -> T.Text
toShortText month =
  case month of
    January ->
      "Jan"

    February ->
      "Feb"

    March ->
      "Mar"

    April ->
      "Apr"

    May ->
      "May"

    June ->
      "Jun"

    July ->
      "Jul"

    August ->
      "Aug"

    September ->
      "Sep"

    October ->
      "Oct"

    November ->
      "Nov"

    December ->
      "Dec"

toInteger :: Month -> Integer
toInteger m =
  case m of
    January   -> 1
    February  -> 2
    March     -> 3
    April     -> 4
    May       -> 5
    June      -> 6
    July      -> 7
    August    -> 8
    September -> 9
    October   -> 10
    November  -> 11
    December  -> 12

fromInteger :: Integer -> Maybe Month
fromInteger n =
  case n of
    1  -> Just January
    2  -> Just February
    3  -> Just March
    4  -> Just April
    5  -> Just May
    6  -> Just June
    7  -> Just July
    8  -> Just August
    9  -> Just September
    10 -> Just October
    11 -> Just November
    12 -> Just December
    _  -> Nothing
