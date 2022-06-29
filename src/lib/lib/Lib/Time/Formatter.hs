{-# LANGUAGE OverloadedStrings #-}

module Lib.Time.Formatter
    ( DateResolution (..)
    , Formatter
    , TimeResolution (..)
    , append
    , concat
    , dayOfMonth
    , empty
    , format
    , hour
    , intercalate
    , iso8601Date
    , iso8601DateAndTime
    , iso8601Time
    , millisecond
    , minute
    , month
    , monthNumber
    , padLeft
    , second
    , shortMonth
    , shortWeekday
    , static
    , weekday
    , weekdayNumber
    , year
    ) where

import qualified Data.List        as List
import qualified Data.Text        as T
import qualified Lib.Text         as T
import           Lib.Time         (Time)
import qualified Lib.Time         as Time
import qualified Lib.Time.Month   as Month
import qualified Lib.Time.Weekday as Weekday
import           Prelude          hiding (concat)



-- FORMATTER


newtype Formatter
  = Formatter (Time.Zone -> Time -> T.Text)


format :: Formatter -> Time.Zone -> Time -> T.Text
format (Formatter f) =
  f



-- BASIC FORMATTERS


empty :: Formatter
empty =
  Formatter $ \_ _ -> ""


static :: T.Text -> Formatter
static s =
  Formatter $ \_ _ -> s



-- ISO-8601 FORMATTERS


data DateResolution = Month | Day


data TimeResolution = Hour | Minute | Second | Millisecond


iso8601DateAndTime :: DateResolution -> TimeResolution -> Formatter
iso8601DateAndTime dateResolution timeResolution =
  concat
    [ iso8601Date dateResolution
    , static "T"
    , iso8601Time timeResolution
    ]


iso8601Date :: DateResolution -> Formatter
iso8601Date resolution =
  let
    separator =
      static "-"

    year_ =
      padLeft 4 '0' year

    month_ =
      padLeft 2 '0' monthNumber

    day_ =
      padLeft 2 '0' dayOfMonth
  in
  concat $
    List.intersperse separator $
      case resolution of
        Month ->
          [ year_, month_ ]

        Day ->
          [ year_, month_, day_ ]


iso8601Time :: TimeResolution -> Formatter
iso8601Time resolution =
  let
    separator =
      static ":"

    hour_ =
      padLeft 2 '0' hour

    minute_ =
      padLeft 2 '0' minute

    second_ =
      padLeft 2 '0' second

    millisecond_ =
      padLeft 3 '0' millisecond
  in
  concat $
    List.intersperse separator $
      case resolution of
        Hour ->
          [ hour_ ]

        Minute ->
          [ hour_, minute_ ]

        Second ->
          [ hour_, minute_, second_ ]

        Millisecond ->
          [ hour_
          , minute_
          , concat
            [ second_
            , static "."
            , millisecond_
            ]
          ]



-- INDIVIDUAL UNIT FORMATTERS


year :: Formatter
year =
  integralFormatter Time.getYear


month :: Formatter
month =
  accessorFormatter Month.toText Time.getMonth


shortMonth :: Formatter
shortMonth =
  accessorFormatter Month.toShortText Time.getMonth


monthNumber :: Formatter
monthNumber =
  integralFormatter (\zone time -> Month.toInteger $ Time.getMonth zone time)


weekday :: Formatter
weekday =
  accessorFormatter Weekday.toText Time.getWeekday


shortWeekday :: Formatter
shortWeekday =
  accessorFormatter Weekday.toShortText Time.getWeekday


weekdayNumber :: Formatter
weekdayNumber =
  integralFormatter (\zone time -> Weekday.toInteger $ Time.getWeekday zone time)


dayOfMonth :: Formatter
dayOfMonth =
  integralFormatter Time.getDayOfMonth


hour :: Formatter
hour =
  integralFormatter Time.getHour


minute :: Formatter
minute =
  integralFormatter Time.getMinute


second :: Formatter
second =
  integralFormatter Time.getSecond


millisecond :: Formatter
millisecond =
  integralFormatter Time.getMillisecond



-- COMBINATORS


append :: Formatter -> Formatter -> Formatter
append (Formatter a) (Formatter b) =
  Formatter $
    \zone time ->
      T.concat
        [ a zone time
        , b zone time
        ]


concat :: [Formatter] -> Formatter
concat =
  foldr append empty


intercalate :: Formatter -> [Formatter] -> Formatter
intercalate separator =
  concat . List.intersperse separator


padLeft :: Int -> Char -> Formatter -> Formatter
padLeft n char (Formatter f) =
  Formatter $ \zone time -> T.padLeft n char $ f zone time



-- INTERNAL HELPERS


accessorFormatter :: (a -> T.Text) -> (Time.Zone -> Time -> a) -> Formatter
accessorFormatter toText accessor =
  Formatter $ \zone time -> toText $ accessor zone time


integralFormatter :: Integral a => (Time.Zone -> Time -> a) -> Formatter
integralFormatter accessor =
  let
    toText =
      T.pack . show . toInteger
  in
  accessorFormatter toText accessor
