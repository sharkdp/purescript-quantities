module Data.Units.NonStandard where

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI

import Prelude ((*))

minute :: DerivedUnit
minute = makeNonStandard "minute" "min" second 60.0

hour :: DerivedUnit
hour = makeNonStandard "hour" "h" second 3600.0

day :: DerivedUnit
day = makeNonStandard "day" "d" second (24.0 * 3600.0)

inch :: DerivedUnit
inch = makeNonStandard "inch" "in" meter 0.0254

foot :: DerivedUnit
foot = makeNonStandard "foot" "ft" meter 0.3048

mile :: DerivedUnit
mile = makeNonStandard "mile" "mi" meter 1609.344
