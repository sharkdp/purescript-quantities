-- | A collection of units from the United States customary system.
module Data.Units.USCustomary where

import Data.Units (DerivedUnit, makeNonStandard, (.^))
import Data.Units.SI (meter)

-- | Unit of volume, the US liquid gallon,
-- | *1 gal = 0.003785411784 m^3 = 231 in^3*
gallon ∷ DerivedUnit
gallon = makeNonStandard "gallon" "gal" 0.003785411784 (meter .^ 3.0)

-- | Unit of volume, the US liquid pint, *1 pint = 1/8·gal*.
pint ∷ DerivedUnit
pint = makeNonStandard "pint" "pint" 0.000473176473 (meter .^ 3.0)

-- | Unit of volume, the US cup, *1 cup = 1/2·pint*.
cup ∷ DerivedUnit
cup = makeNonStandard "cup" "cup" 0.0002365882365 (meter .^ 3.0)

-- | Unit of volume, the US tablespoon, *1 tablespoon = 1/16·cup*.
tablespoon ∷ DerivedUnit
tablespoon = makeNonStandard "tablespoon" "tablespoon" 0.00001478676478125 (meter .^ 3.0)

-- | Unit of volume, the US teaspoon, *1 teaspoon = 1/3·tablespoon*.
teaspoon ∷ DerivedUnit
teaspoon = makeNonStandard "teaspoon" "teaspoon" 0.00000492892159375 (meter .^ 3.0)

-- | Unit of volume, the US fluid ounce, *1 floz = 2 tablespoon*.
fluidounce ∷ DerivedUnit
fluidounce = makeNonStandard "fluidounce" "floz" 0.0000295735295625 (meter .^ 3.0)

-- | Unit of volume, the US hogshead, *1 hogshead = 63 gal*.
hogshead ∷ DerivedUnit
hogshead = makeNonStandard "hogshead" "hogshead" 0.238480942392 (meter .^ 3.0)

-- | Unit of length, the US rod, *1 rod = 16.5 ft*.
rod ∷ DerivedUnit
rod = makeNonStandard "rod" "rod" 5.0292 meter
