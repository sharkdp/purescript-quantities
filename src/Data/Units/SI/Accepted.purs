-- | A collection of non-SI units that are accepted by the SI brochure.
module Data.Units.SI.Accepted where

import Prelude

import Data.Number (pi)

import Data.Units (DerivedUnit, makeStandard, makeNonStandard, (.^), (./),
                   unity)
import Data.Units.SI (meter, second, gram)

-- | 'Dimensionless' unit for angles *360 degree = 2·pi rad*.
degree ∷ DerivedUnit
degree = makeNonStandard "degree" "°" (pi / 180.0) unity

-- | Unit of area, *1 ha = 10000 m²*.
hectare ∷ DerivedUnit
hectare = makeNonStandard "hectare" "ha" 10000.0 (meter .^ 2.0)

-- | Unit of volume, *1 L = 1 dm³*.
liter ∷ DerivedUnit
liter = makeNonStandard "liter" "L" 0.001 (meter .^ 3.0)

-- | Unit of mass, *1 t = 10³ km*.
tonne ∷ DerivedUnit
tonne = makeNonStandard "tonne" "ton" 1000000.0 gram

-- | Unit of energy, *1 eV = 1.60217653·10^(−19) J*.
electronvolt ∷ DerivedUnit
electronvolt = makeNonStandard "electronvolt" "eV" 1.60217653e-16 (gram <> meter .^ 2.0 ./ second .^ 2.0)

-- | Unit for expressing ratios of two values of a physical quantity.
bel ∷ DerivedUnit
bel = makeStandard "bel" "bel"

-- | Unit of length, *1 AU = 1.495978707·10^11 m*.
astronomicalUnit ∷ DerivedUnit
astronomicalUnit = makeNonStandard "astronomical unit" "AU" 1.495978707e11 meter

-- | Unit of pressure, *1 bar = 10^5 Pa*.
bar ∷ DerivedUnit
bar = makeNonStandard "bar" "bar" 1.0e8 (gram ./ (meter <> second .^ 2.0))

-- | Unit of length, *1 Å = 10^(-10) m*.
angstrom ∷ DerivedUnit
angstrom = makeNonStandard "Ångström" "Å" 1.0e-10 meter

-- | Unit of area, *1 barn = 10^(-28) m^2*.
barn ∷ DerivedUnit
barn = makeNonStandard "barn" "barn" 1.0e-28 (meter .^ 2.0)
