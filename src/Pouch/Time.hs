{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Pouch.Time
    ( Days
    , Hours
    , Microseconds
    , Milliseconds
    , Minutes
    , Month (..)
    , Nanoseconds
    , Picoseconds
    , Seconds
    , Time
    , Weekday (..)
    , Weeks
    , Years
    , Zone
    , addDays
    , addHours
    , addMicroseconds
    , addMilliseconds
    , addMinutes
    , addNanoseconds
    , addPicoseconds
    , addSeconds
    , addWeeks
    , fromDays
    , fromHours
    , fromMicroseconds
    , fromMilliseconds
    , fromMinutes
    , fromNanoseconds
    , fromPicoseconds
    , fromPosix
    , fromSeconds
    , fromWeeks
    , getDayOfMonth
    , getExternalZone
    , getHour
    , getMillisecond
    , getMinute
    , getMonth
    , getSecond
    , getWeekday
    , getYear
    , here
    , now
    , toDays
    , toHours
    , toMicroseconds
    , toMilliseconds
    , toMinutes
    , toNanoseconds
    , toPicoseconds
    , toPosix
    , toSeconds
    , toWeeks
    , utc
    ) where

import           Data.Binary           (Binary)
import qualified Data.Binary           as Binary
import qualified Data.Fixed            as Fixed
import qualified Data.Text             as T
import qualified Data.Time             as Time
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Pouch.Text              as T
import           Pouch.Time.Month        (Month (..))
import qualified Pouch.Time.Month        as Month
import           Pouch.Time.Weekday      (Weekday (..))
import qualified Pouch.Time.Weekday      as Weekday

-- * Types

newtype Time
  = Time POSIXTime.POSIXTime
  deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)

instance Binary Time where
  get = fromPicoseconds <$> Binary.get
  put = Binary.put . toPicoseconds

newtype Years
  = Years Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Years where
  get = Years <$> Binary.get
  put (Years n) = Binary.put n

newtype Weeks
  = Weeks Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Weeks where
  get = Weeks <$> Binary.get
  put (Weeks n) = Binary.put n

newtype Days
  = Days Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Days where
  get = Days <$> Binary.get
  put (Days n) = Binary.put n

newtype Hours
  = Hours Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Hours where
  get = Hours <$> Binary.get
  put (Hours n) = Binary.put n

newtype Minutes
  = Minutes Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Minutes where
  get = Minutes <$> Binary.get
  put (Minutes n) = Binary.put n

newtype Seconds
  = Seconds Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Seconds where
  get = Seconds <$> Binary.get
  put (Seconds n) = Binary.put n

newtype Milliseconds
  = Milliseconds Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Milliseconds where
  get = Milliseconds <$> Binary.get
  put (Milliseconds n) = Binary.put n

newtype Microseconds
  = Microseconds Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Microseconds where
  get = Microseconds <$> Binary.get
  put (Microseconds n) = Binary.put n

newtype Nanoseconds
  = Nanoseconds Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Nanoseconds where
  get = Nanoseconds <$> Binary.get
  put (Nanoseconds n) = Binary.put n

newtype Picoseconds
  = Picoseconds Integer
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance Binary Picoseconds where
  get = Picoseconds <$> Binary.get
  put (Picoseconds n) = Binary.put n

-- * Current Time

now :: IO Time
now = Time <$> POSIXTime.getPOSIXTime

-- * Time Zones


newtype Zone
  = Zone Time.TimeZone
  deriving (Eq, Ord)

instance Show Zone where
  show (Zone z) = show z

instance Binary Zone where
  get = Zone <$> Binary.get
  put (Zone z) = Binary.put z

instance Binary Time.TimeZone where
  get = Time.TimeZone <$> Binary.get <*> Binary.get <*> Binary.get
  put (Time.TimeZone {..}) = do
    Binary.put timeZoneMinutes
    Binary.put timeZoneSummerOnly
    Binary.put timeZoneName


utc :: Zone
utc = Zone Time.utc

here :: IO Zone
here = Zone <$> Time.getCurrentTimeZone

getExternalZone :: Zone -> Time.TimeZone
getExternalZone (Zone zone) = zone

-- * Human-readable

getYear :: Zone -> Time -> Years
getYear zone time = Years year
  where
    (year, _, _) = getGregorian zone time

getMonth :: Zone -> Time -> Month
getMonth zone time = month
  where
    (_, monthInt, _) = getGregorian zone time
    -- Run-time exception if external Time library returns invalid int.
    (Just month) = Month.fromInteger $ fromIntegral monthInt

getWeekday :: Zone -> Time -> Weekday
getWeekday zone time = Weekday.fromExternalDayOfWeek $ Time.dayOfWeek $ Time.localDay $ getLocalTime zone time

getDayOfMonth :: Zone -> Time -> Days
getDayOfMonth zone time = Days $ fromIntegral day
  where
    (_, _, day) = getGregorian zone time

getHour :: Zone -> Time -> Hours
getHour zone time
  = Hours
  $ fromIntegral
  $ Time.todHour
  $ getTimeOfDay zone time

getMinute :: Zone -> Time -> Minutes
getMinute zone time
  = Minutes
  $ fromIntegral
  $ Time.todMin
  $ getTimeOfDay zone time

getSecond :: Zone -> Time -> Seconds
getSecond zone time
  = Seconds
  $ floor
  $ Time.todSec -- Picoseconds
  $ getTimeOfDay zone time

getMillisecond :: Zone -> Time -> Milliseconds
getMillisecond zone time
  = Milliseconds
  $ flip mod 1000
  $ floor
  $ (* (10 ^ 3))
  $ Time.todSec -- Picoseconds
  $ getTimeOfDay zone time

-- ** Human-Readable Helpers

-- | Returns '(Year, MonthOfYear, DayOfMonth)'.
getGregorian :: Zone -> Time -> (Integer, Int, Int)
getGregorian zone time = Time.toGregorian $ Time.localDay $ getLocalTime zone time

getLocalTime :: Zone -> Time -> Time.LocalTime
getLocalTime (Zone zone) (Time time) = Time.utcToLocalTime zone utcTime
  where
    utcTime = POSIXTime.posixSecondsToUTCTime time

getTimeOfDay :: Zone -> Time -> Time.TimeOfDay
getTimeOfDay zone time = Time.localTimeOfDay $ getLocalTime zone time

-- * Arithmetic

addWeeks = add fromWeeks
addDays = add fromDays
addHours = add fromHours
addMinutes = add fromMinutes
addSeconds = add fromSeconds
addMilliseconds = add fromMilliseconds
addMicroseconds = add fromMicroseconds
addNanoseconds = add fromNanoseconds
addPicoseconds = add fromPicoseconds

add :: (a -> Time) -> a -> Time -> Time
add toTime a time = time + toTime a

-- * Serialization

toWeeks :: Time -> Weeks
toWeeks = fromInteger . (`div` 7) . toInteger . toDays

fromWeeks :: Weeks -> Time
fromWeeks = fromDays . fromInteger . (* 7) . toInteger

toDays :: Time -> Days
toDays = fromInteger . (`div` 24) . toInteger . toHours

fromDays :: Days -> Time
fromDays = fromHours . fromInteger . (* 24) . toInteger

toHours :: Time -> Hours
toHours = fromInteger . (`div` 60) . toInteger . toMinutes

fromHours :: Hours -> Time
fromHours = fromMinutes . fromInteger . (* 60) . toInteger

toMinutes :: Time -> Minutes
toMinutes = fromInteger . (`div` 60) . toInteger . toSeconds

fromMinutes :: Minutes -> Time
fromMinutes = fromSeconds . fromInteger . (* 60) . toInteger

toSeconds :: Time -> Seconds
toSeconds = fromInteger . (`div` (10 ^ 3)) . toInteger . toMilliseconds

fromSeconds :: Seconds -> Time
fromSeconds = fromMilliseconds . fromInteger . (* (10 ^ 3)) . toInteger

toMilliseconds :: Time -> Milliseconds
toMilliseconds = fromInteger . (`div` (10 ^ 3)) . toInteger . toMicroseconds

fromMilliseconds :: Milliseconds -> Time
fromMilliseconds = fromMicroseconds . fromInteger . (* (10 ^ 3)) . toInteger

toMicroseconds :: Time -> Microseconds
toMicroseconds = fromInteger . (`div` (10 ^ 3)) . toInteger . toNanoseconds

fromMicroseconds :: Microseconds -> Time
fromMicroseconds = fromNanoseconds . fromInteger . (* (10 ^ 3)) . toInteger

toNanoseconds :: Time -> Nanoseconds
toNanoseconds = fromInteger . (`div` (10 ^ 3)) . toInteger . toPicoseconds

fromNanoseconds :: Nanoseconds -> Time
fromNanoseconds = fromPicoseconds . fromInteger . (* (10 ^ 3)) . toInteger

toPicoseconds :: Time -> Picoseconds
toPicoseconds = Picoseconds . floor . (* (10 ^ 12)) . Time.nominalDiffTimeToSeconds . toPosix

fromPicoseconds :: Picoseconds -> Time
fromPicoseconds = fromPosix . Time.secondsToNominalDiffTime . (/ (10 ^ 12)) . realToFrac

toPosix :: Time -> POSIXTime.POSIXTime
toPosix (Time time) = time

fromPosix :: POSIXTime.POSIXTime -> Time
fromPosix = Time
