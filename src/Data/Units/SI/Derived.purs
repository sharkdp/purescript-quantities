-- | A collection of units that are derived from SI units.
module Data.Units.SI.Derived where

import Data.Units (DerivedUnit, (./), (.^), unity, makeNonStandard)
import Data.Units.SI

import Prelude ((<>))

-- | Unit for frequency, *1 Hz = 1 / s*.
hertz :: DerivedUnit
hertz = makeNonStandard "hertz" "Hz" 1.0 (unity ./ second)

-- | 'Dimensionless' unit for angles *1 rad = 1m / 1m*.
radian :: DerivedUnit
radian = unity

-- | 'Dimensionless' unit for solid angles *1 sr = 1m² / 1m²*.
steradian :: DerivedUnit
steradian = unity

-- | Unit for force, *1N = 1kg⋅m/s²*.
newton :: DerivedUnit
newton = makeNonStandard "newton" "N" 1.0 (kilogram <> meter ./ second .^ 2.0)

-- | Unit for energy, *1J = 1N·m*.
joule :: DerivedUnit
joule = makeNonStandard "joule" "J" 1.0 (kilogram <> meter .^ 2.0 ./ second .^ 2.0)

-- | Unit for power, *1W = 1J/s*.
watt :: DerivedUnit
watt = makeNonStandard "watt" "W" 1.0 (kilogram <> meter .^ 2.0 ./ second .^ 3.0)
