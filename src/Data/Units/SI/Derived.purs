-- | A collection of units that are derived from SI units.
module Data.Units.SI.Derived where

import Prelude

import Data.Units (DerivedUnit, (./), (.^), unity, makeNonStandard)
import Data.Units.SI (ampere, candela, gram, meter, mole, second)

-- | 'Dimensionless' unit for angles *1 rad = 1m / 1m*.
radian :: DerivedUnit
radian = unity

-- | 'Dimensionless' unit for solid angles *1 sr = 1m² / 1m²*.
steradian :: DerivedUnit
steradian = unity

-- | Unit of frequency, *1 Hz = 1 / s*.
hertz :: DerivedUnit
hertz = makeNonStandard "hertz" "Hz" 1.0 (unity ./ second)

-- | Unit of force, *1N = 1kg⋅m/s²*.
newton :: DerivedUnit
newton = makeNonStandard "newton" "N" 1000.0 (gram <> meter ./ second .^ 2.0)

-- | Unit of pressure, *1Pa = 1N/m²*.
pascal :: DerivedUnit
pascal = makeNonStandard "pascal" "Pa" 1000.0 (gram ./ (meter <> second .^ 2.0))

-- | Unit of energy, *1J = 1N·m*.
joule :: DerivedUnit
joule = makeNonStandard "joule" "J" 1000.0 (gram <> meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of power, *1W = 1J/s*.
watt :: DerivedUnit
watt = makeNonStandard "watt" "W" 1000.0 (gram <> meter .^ 2.0 ./ second .^ 3.0)

-- | Unit of electric charge, *1C = 1A·s*.
coulomb :: DerivedUnit
coulomb = makeNonStandard "coulomb" "C" 1.0 (ampere <> second)

-- | Unit of voltage, *1V = 1W/A*.
volt :: DerivedUnit
volt = makeNonStandard "volt" "V" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 3.0 <> ampere))

-- | Unit of capacitance, *1F = 1C/V*.
farad :: DerivedUnit
farad = makeNonStandard "farad" "F" 0.001 (second .^ 4.0 <> ampere .^ 2.0 ./ (gram .^ 1.0 <> meter .^ 2.0))

-- | Unit of electric resistance, *1Ω = 1V/A*.
ohm :: DerivedUnit
ohm = makeNonStandard "ohm" "Ω" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 3.0 <> ampere .^ 2.0))

-- | Unit of electrical conductance, *1S = 1A/V*.
siemens :: DerivedUnit
siemens = makeNonStandard "siemens" "S" 0.001 (second .^ 3.0 <> ampere .^ 2.0 ./ (gram <> meter .^ 2.0))

-- | Unit of magnetic flux, *1Wb = 1V·s*.
weber :: DerivedUnit
weber = makeNonStandard "weber" "Wb" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 2.0 <> ampere))

-- | Unit of magnetic flux density, *1T = 1Wb/m²*.
tesla :: DerivedUnit
tesla = makeNonStandard "tesla" "T" 1000.0 (gram ./ (second .^ 2.0 <> ampere))

-- | Unit of inductance, *1H = 1Wb/A*.
henry :: DerivedUnit
henry = makeNonStandard "henry" "H" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 2.0 <> ampere .^ 2.0))

-- | Unit of luminous flux, *1lm = 1cd·sr*.
lumen :: DerivedUnit
lumen = makeNonStandard "lumen" "lm" 1.0 candela

-- | Unit of illuminance, *1lx = 1lm/m²*.
lux :: DerivedUnit
lux = makeNonStandard "lux" "lx" 1.0 (candela ./ meter .^ 2.0)

-- | Unit of radioactivity (decays per time), *1Bq = 1/s*.
becquerel :: DerivedUnit
becquerel = makeNonStandard "becquerel" "Bq" 1.0 (second .^ (-1.0))

-- | Unit of absorbed dose, *1Gy = 1J/kg*.
gray :: DerivedUnit
gray = makeNonStandard "gray" "Gy" 1.0 (meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of equivalent dose, *1Sv = 1J/kg*.
sievert :: DerivedUnit
sievert = makeNonStandard "sievert" "Sv" 1.0 (meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of catalytic activity, *1kat = 1mol/s*.
katal :: DerivedUnit
katal = makeNonStandard "katal" "kat" 1.0 (mole ./ second)
