{-# LANGUAGE OverloadedStrings #-}

module Pouch.Time.Weekday
    ( Weekday (..)
    , fromExternalDayOfWeek
    , fromInteger
    , toExternalDayOfWeek
    , toInteger
    , toShortText
    , toText
    ) where

import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.Text   as T
import qualified Data.Time   as TimeExt
import           Prelude     hiding (fromInteger, toInteger)


data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving
    ( Enum
    , Eq
    , Show
    )

instance Binary Weekday where
  get = do
    id' <- Binary.getWord8
    return $ case id' of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
  put month =
    case month of
      Monday    -> Binary.putWord8 0
      Tuesday   -> Binary.putWord8 1
      Wednesday -> Binary.putWord8 2
      Thursday  -> Binary.putWord8 3
      Friday    -> Binary.putWord8 4
      Saturday  -> Binary.putWord8 5
      Sunday    -> Binary.putWord8 6


toText :: Weekday -> T.Text
toText weekday =
  case weekday of
    Monday ->
      "Monday"

    Tuesday ->
      "Tuesday"

    Wednesday ->
      "Wednesday"

    Thursday ->
      "Thursday"

    Friday ->
      "Friday"

    Saturday ->
      "Saturday"

    Sunday ->
      "Sunday"

toShortText :: Weekday -> T.Text
toShortText weekday =
  case weekday of
    Monday ->
      "Mon"

    Tuesday ->
      "Tue"

    Wednesday ->
      "Wed"

    Thursday ->
      "Thu"

    Friday ->
      "Fri"

    Saturday ->
      "Sat"

    Sunday ->
      "Sun"


toInteger :: Weekday -> Integer
toInteger weekday =
  case weekday of
    Monday ->
      1

    Tuesday ->
      2

    Wednesday ->
      3

    Thursday ->
      4

    Friday ->
      5

    Saturday ->
      6

    Sunday ->
      7


fromInteger :: Int -> Maybe Weekday
fromInteger n =
  case n of
    1 ->
      Just Monday

    2 ->
      Just Tuesday

    3 ->
      Just Wednesday

    4 ->
      Just Thursday

    5 ->
      Just Friday

    6 ->
      Just Saturday

    7 ->
      Just Sunday

    _ ->
      Nothing


toExternalDayOfWeek :: Weekday -> TimeExt.DayOfWeek
toExternalDayOfWeek weekday =
  case weekday of
    Monday ->
      TimeExt.Monday

    Tuesday ->
      TimeExt.Tuesday

    Wednesday ->
      TimeExt.Wednesday

    Thursday ->
      TimeExt.Thursday

    Friday ->
      TimeExt.Friday

    Saturday ->
      TimeExt.Saturday

    Sunday ->
      TimeExt.Sunday


fromExternalDayOfWeek :: TimeExt.DayOfWeek -> Weekday
fromExternalDayOfWeek ext =
  case ext of
    TimeExt.Monday ->
      Monday

    TimeExt.Tuesday ->
      Tuesday

    TimeExt.Wednesday ->
      Wednesday

    TimeExt.Thursday ->
      Thursday

    TimeExt.Friday ->
      Friday

    TimeExt.Saturday ->
      Saturday

    TimeExt.Sunday ->
      Sunday
