-- | A collection of units from the imperial system.
module Data.Units.Imperial where

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI

-- | Unit of length, *1 in = 0.0254 m*.
inch ∷ DerivedUnit
inch = makeNonStandard "inch" "in" 0.0254 meter

-- | Unit of length, *1 ft = 0.3048 m*.
foot ∷ DerivedUnit
foot = makeNonStandard "foot" "ft" 0.3048 meter

-- | Unit of length, *1 yd = 0.9144 m*.
yard ∷ DerivedUnit
yard = makeNonStandard "yard" "yd" 0.9144 meter

-- | Unit of length, *1 mi = 1609.344 m*.
mile ∷ DerivedUnit
mile = makeNonStandard "mile" "mi" 1609.344 meter

-- | Unit of mass, *1 oz = 28.35 g*.
ounce ∷ DerivedUnit
ounce = makeNonStandard "ounce" "oz" 28.35 gram

-- | Unit of mass, *1 lb = 453.6 g*.
pound ∷ DerivedUnit
pound = makeNonStandard "pound" "lb" 453.6 gram
