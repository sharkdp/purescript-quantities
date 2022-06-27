module Data.Units.Nautical where

import Prelude

import Data.Units (DerivedUnit, makeNonStandard, (./))
import Data.Units.SI (second, meter)

knot ∷ DerivedUnit
knot = makeNonStandard "knot" "kn" (1852.0 / 3600.0) (meter ./ second)

nauticalMile ∷ DerivedUnit
nauticalMile = makeNonStandard "nautical mile" "M" 1852.0 meter
