-- | A collection of units from the imperial system.
module Data.Units.Imperial where

import Data.Units (DerivedUnit, makeNonStandard, (.^))
import Data.Units.SI

-- | Unit of length, *1in = 0.0254m*.
inch ∷ DerivedUnit
inch = makeNonStandard "inch" "in" 0.0254 meter

-- | Unit of length, *1ft = 0.3048m*.
foot ∷ DerivedUnit
foot = makeNonStandard "foot" "ft" 0.3048 meter

-- | Unit of length, *1yd = 0.9144m*.
yard ∷ DerivedUnit
yard = makeNonStandard "yard" "yd" 0.9144 meter

-- | Unit of length, *1mi = 1609.344m*.
mile ∷ DerivedUnit
mile = makeNonStandard "mile" "mi" 1609.344 meter

-- | Unit of mass, *1oz = 28.35g*.
ounce ∷ DerivedUnit
ounce = makeNonStandard "ounce" "oz" 28.35 gram

-- | Unit of mass, *1lb = 453.6g*.
pound ∷ DerivedUnit
pound = makeNonStandard "pound" "lb" 453.6 gram

-- | Unit of Volume, the US liquid gallon *1gal = 0.003785411784m^3*
gallon ∷ DerivedUnit
gallon = makeNonStandard "gallon" "gal" 0.003785411784 (meter .^ 3.0)
