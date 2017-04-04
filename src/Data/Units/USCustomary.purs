-- | A collection of units from the United States customary system.
module Data.Units.USCustomary where

import Data.Units (DerivedUnit, makeNonStandard, (.^))
import Data.Units.SI

-- | Unit of Volume, the US liquid gallon, *1gal = 0.003785411784m^3 = 231in^3*
gallon ∷ DerivedUnit
gallon = makeNonStandard "gallon" "gal" 0.003785411784 (meter .^ 3.0)

-- | Unit of Volume, the US liquid pint, *1pint = 1/8·gal*.
pint ∷ DerivedUnit
pint = makeNonStandard "pint" "pint" 0.000473176473 (meter .^ 3.0)

-- | Unit of Volume, the US cup, *1cup = 1/2·pint*.
cup ∷ DerivedUnit
cup = makeNonStandard "cup" "cup" 0.0002365882365 (meter .^ 3.0)

-- | Unit of Volume, the US tablespoon, *1tablespoon = 1/16·cup*.
tablespoon ∷ DerivedUnit
tablespoon = makeNonStandard "tablespoon" "tablespoon" 0.00001478676478125 (meter .^ 3.0)

-- | Unit of Volume, the US teaspoon, *1teaspoon = 1/3·tablespoon*.
teaspoon ∷ DerivedUnit
teaspoon = makeNonStandard "teaspoon" "teaspoon" 0.00000492892159375 (meter .^ 3.0)
