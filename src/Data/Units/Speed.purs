module Data.Units.Speed where

import Data.Units(DerivedUnit, makeNonStandard)

knot :: DerivedUnit
knot = makeNonStandard "knot" "kn" (1852/3600) (meter ./ second)

nauticalMile :: DerivedUnit
nauticalMile = makeNonStandard "nautical mile" "M" (1852) (meter)
