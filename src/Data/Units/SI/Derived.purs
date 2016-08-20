-- | A collection of units that are derived from SI units.
module Data.Units.SI.Derived where

import Data.Units (DerivedUnit, (./), (.^), unity)
import Data.Units.SI

import Prelude ((<>))

-- | Unit for frequency, *1 Hz = 1 / s*.
hertz :: DerivedUnit
hertz = unity ./ second

-- | 'Dimensionless' unit for angles *1 rad = 1m / 1m*.
radian :: DerivedUnit
radian = unity

-- | 'Dimensionless' unit for solid angles *1 sr = 1m² / 1m²*.
steradian :: DerivedUnit
steradian = unity

-- | Unit for force, *1N = 1kg⋅m/s²*.
newton :: DerivedUnit
newton = kilogram <> meter ./ second .^ 2.0

-- | Unit for energy, *1J = 1N·m*.
joule :: DerivedUnit
joule = newton <> meter

-- | Unit for power, *1W = 1J/s*.
watt :: DerivedUnit
watt = joule ./ second
