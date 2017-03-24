-- | A collection of non-SI units that are accepted by the SI brochure.
module Data.Units.SI.Accepted where

import Prelude

import Math (pi)

import Data.Units (DerivedUnit, makeNonStandard, (.^), (./))
import Data.Units.SI (meter, second, gram)
import Data.Units.SI.Derived (radian)

-- | 'Dimensionless' unit for angles *360 degree = 2 pi rad*.
degree ∷ DerivedUnit
degree = makeNonStandard "degree" "°" (pi / 180.0) radian

-- | Unit of area, *1ha = 10000m²*.
hectare ∷ DerivedUnit
hectare = makeNonStandard "hectare" "ha" 10000.0 (meter .^ 2.0)

-- | Unit of volume, *1L = 1dm³*.
liter ∷ DerivedUnit
liter = makeNonStandard "liter" "L" 0.001 (meter .^ 3.0)

-- | Unit of mass, *1t = 10³ km*.
tonne ∷ DerivedUnit
tonne = makeNonStandard "tonne" "ton" 1000000.0 gram

-- | Unit of energy, *1eV = 1.60217653·10^(−19) J*.
electronvolt ∷ DerivedUnit
electronvolt = makeNonStandard "electronvolt" "eV" 1.60217653e-16 (gram <> meter .^ 2.0 ./ second .^ 2.0)
