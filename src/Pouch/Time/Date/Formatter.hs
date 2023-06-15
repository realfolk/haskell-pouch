{-# LANGUAGE OverloadedStrings #-}

module Pouch.Time.Date.Formatter
    ( DateResolution (..)
    , Formatter
    , append
    , concat
    , dayOfMonth
    , empty
    , format
    , intercalate
    , iso8601Date
    , month
    , monthNumber
    , padLeft
    , shortMonth
    , shortWeekday
    , static
    , weekday
    , weekdayNumber
    , year
    ) where

import qualified Data.List        as List
import qualified Data.Text        as T
import qualified Pouch.Text         as T
import           Pouch.Time.Date    (Date)
import qualified Pouch.Time.Date    as Date
import qualified Pouch.Time.Month   as Month
import qualified Pouch.Time.Weekday as Weekday
import           Prelude          hiding (concat)



-- FORMATTER


newtype Formatter
  = Formatter (Date -> T.Text)


format :: Formatter -> Date -> T.Text
format (Formatter f) =
  f



-- BASIC FORMATTERS


empty :: Formatter
empty =
  Formatter $ const ""


static :: T.Text -> Formatter
static s =
  Formatter $ const s



-- ISO-8601 FORMATTERS


data DateResolution = Month | Day

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



-- INDIVIDUAL UNIT FORMATTERS


year :: Formatter
year =
  integralFormatter Date.getYear


month :: Formatter
month =
  accessorFormatter Month.toText Date.getMonth


shortMonth :: Formatter
shortMonth =
  accessorFormatter Month.toShortText Date.getMonth


monthNumber :: Formatter
monthNumber =
  integralFormatter (Month.toInteger . Date.getMonth)


weekday :: Formatter
weekday =
  accessorFormatter Weekday.toText Date.getWeekday


shortWeekday :: Formatter
shortWeekday =
  accessorFormatter Weekday.toShortText Date.getWeekday


weekdayNumber :: Formatter
weekdayNumber =
  integralFormatter (Weekday.toInteger . Date.getWeekday)


dayOfMonth :: Formatter
dayOfMonth =
  integralFormatter Date.getDayOfMonth



-- COMBINATORS


append :: Formatter -> Formatter -> Formatter
append (Formatter a) (Formatter b) =
  Formatter $
    \date ->
      T.concat
        [ a date
        , b date
        ]


concat :: [Formatter] -> Formatter
concat =
  foldr append empty


intercalate :: Formatter -> [Formatter] -> Formatter
intercalate separator =
  concat . List.intersperse separator


padLeft :: Int -> Char -> Formatter -> Formatter
padLeft n char (Formatter f) =
  Formatter (T.padLeft n char . f)



-- INTERNAL HELPERS


accessorFormatter :: (a -> T.Text) -> (Date -> a) -> Formatter
accessorFormatter toText accessor =
  Formatter (toText . accessor)


integralFormatter :: Integral a => (Date -> a) -> Formatter
integralFormatter accessor =
  let
    toText =
      T.pack . show . toInteger
  in
  accessorFormatter toText accessor
