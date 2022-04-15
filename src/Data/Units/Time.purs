-- | A collection of units for durations of time.
module Data.Units.Time where

import Prelude

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI (second)

-- | Unit of time, *1 min = 60 sec*.
minute ∷ DerivedUnit
minute = makeNonStandard "minute" "min" 60.0 second

-- | Unit of time, *1 hour = 60 min*.
hour ∷ DerivedUnit
hour = makeNonStandard "hour" "h" 3600.0 second

-- | Unit of time, *1 day = 24 hour*.
day ∷ DerivedUnit
day = makeNonStandard "day" "d" (24.0 * 3600.0) second

-- | Unit of time, *1 week = 7 days*.
week ∷ DerivedUnit
week = makeNonStandard "week" "week" (7.0 * 24.0 * 3600.0) second

-- | Unit of time, *1 month = 30 days + 10 hours + 29 minutes + 10 seconds*.
month ∷ DerivedUnit
month = makeNonStandard "month" "month" ((30.0 * 24.0 + 10.485) * 3600.0) second

-- | Unit of time, *1 year = 365.2425 days* (Gregorian year).
year ∷ DerivedUnit
year = makeNonStandard "year" "year" (365.2425 * 24.0 * 3600.0) second

-- | Unit of time, *1 julianYear = 365.25 days*.
julianYear ∷ DerivedUnit
julianYear = makeNonStandard "julian year" "julianYear" (365.25 * 24.0 * 3600.0) second
