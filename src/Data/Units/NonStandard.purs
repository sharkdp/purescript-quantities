module Data.Units.NonStandard where

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI

minute :: DerivedUnit
minute = makeNonStandard "minute" "min" second 60.0

minutes :: DerivedUnit
minutes = minute

hour :: DerivedUnit
hour = makeNonStandard "hour" "h" second 3600.0

hours :: DerivedUnit
hours = hour

inch :: DerivedUnit
inch = makeNonStandard "inch" "in" meter 0.0254

inches :: DerivedUnit
inches = inch

foot :: DerivedUnit
foot = makeNonStandard "foot" "ft" meter 0.3048

feet :: DerivedUnit
feet = foot

mile :: DerivedUnit
mile = makeNonStandard "mile" "mi" meter 1609.344

miles :: DerivedUnit
miles = mile
