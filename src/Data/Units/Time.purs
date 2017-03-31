-- | A collection of units for durations of time.
module Data.Units.Time where

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI

import Prelude ((*), (+))

-- | Unit of time, *1min = 60sec*.
minute ∷ DerivedUnit
minute = makeNonStandard "minute" "min" 60.0 second

-- | Unit of time, *1hour = 60min*.
hour ∷ DerivedUnit
hour = makeNonStandard "hour" "h" 3600.0 second

-- | Unit of time, *1day = 24hour*.
day ∷ DerivedUnit
day = makeNonStandard "day" "d" (24.0 * 3600.0) second

-- | Unit of time, *1week = 7days*.
week ∷ DerivedUnit
week = makeNonStandard "week" "week" (7.0 * 24.0 * 3600.0) second

-- | Unit of time, *1month = 30days + 10hours*.
month ∷ DerivedUnit
month = makeNonStandard "month" "month" ((30.0 * 24.0 + 10.0) * 3600.0) second

-- | Unit of time, *1year = 365days*.
year ∷ DerivedUnit
year = makeNonStandard "year" "year" (365.0 * 24.0 * 3600.0) second
