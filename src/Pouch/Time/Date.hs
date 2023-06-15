{-# LANGUAGE OverloadedStrings #-}

module Pouch.Time.Date
    ( Date
    , addDays
    , addWeeks
    , currentLocalYear
    , date
    , fromTime
    , getDayOfMonth
    , getMonth
    , getWeekday
    , getYear
    , toTime
    , today
    ) where

import           Data.Binary           (Binary)
import qualified Data.Binary           as Binary
import qualified Data.Text             as T
import qualified Data.Time             as TimeExt
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Pouch.Text              as T
import qualified Pouch.Time              as Time
import qualified Pouch.Time.Month        as Month
import qualified Pouch.Time.Weekday      as Weekday

-- * Types

data Date
  = Date !Time.Years !Time.Month !Time.Days
  deriving (Eq)

instance Ord Date where
  compare a b = compare (toLocalTime a) (toLocalTime b)

instance Binary Date where
  get = Date <$> Binary.get <*> Binary.get <*> Binary.get
  put (Date years month days) = Binary.put years >> Binary.put month >> Binary.put days

date = Date

toTime :: Time.Zone -> Date -> Time.Time
toTime zone date = Time.fromPosix $ POSIXTime.utcTimeToPOSIXSeconds utcTime
  where
    localTime = toLocalTime date
    utcTime = TimeExt.localTimeToUTC (Time.getExternalZone zone) localTime

fromTime :: Time.Zone -> Time.Time -> Date
fromTime zone time =
  Date (Time.getYear zone time) (Time.getMonth zone time) (Time.getDayOfMonth zone time)

-- * Current Date

today :: Time.Zone -> IO Date
today zone = fromTime zone <$> Time.now

currentLocalYear :: IO Time.Years
currentLocalYear = getYear <$> (today =<< Time.here)

-- * Human-readable

getYear :: Date -> Time.Years
getYear (Date y _ _) = y

getMonth :: Date -> Time.Month
getMonth (Date _ m _) = m

getWeekday :: Date -> Time.Weekday
getWeekday = Weekday.fromExternalDayOfWeek . TimeExt.dayOfWeek . TimeExt.localDay . toLocalTime

getDayOfMonth :: Date -> Time.Days
getDayOfMonth (Date _ _ d) = d

-- * Arithmetic

addWeeks :: Time.Weeks -> Date -> Date
addWeeks weeks date = fromLocalTime $ TimeExt.addLocalTime (Time.toPosix (Time.fromWeeks weeks)) $ toLocalTime date

addDays :: Time.Days -> Date -> Date
addDays days date = fromLocalTime $ TimeExt.addLocalTime (Time.toPosix (Time.fromDays days)) $ toLocalTime date

-- * Misc. Helpers

toLocalTime :: Date -> TimeExt.LocalTime
toLocalTime (Date y m d) = TimeExt.LocalTime day TimeExt.midnight
  where
    day = TimeExt.fromGregorian (toInteger y) (fromInteger $ Month.toInteger m) (fromIntegral d)

fromLocalTime :: TimeExt.LocalTime -> Date
fromLocalTime time = Date (fromIntegral y) month (fromIntegral d)
  where
    -- Run-time exception if external Time library returns invalid int.
    (Just month) = Month.fromInteger $ fromIntegral m
    (y, m, d) = TimeExt.toGregorian $ TimeExt.localDay time
