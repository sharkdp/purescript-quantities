-- | A collection of units that are directly derived from SI units and accepted
-- | by the standard.
module Data.Units.SI.Derived where

import Prelude

import Data.Units (DerivedUnit, (./), (.^), unity, makeNonStandard)
import Data.Units.SI (ampere, candela, gram, meter, mole, second)

-- | 'Dimensionless' unit for angles *1 rad = 1 m/m*.
radian ∷ DerivedUnit
radian = makeNonStandard "radian" "rad" 1.0 unity

-- | 'Dimensionless' unit for solid angles *1 sr = 1 m²/m²*.
steradian ∷ DerivedUnit
steradian = makeNonStandard "steradian" "sr" 1.0 unity

-- | Unit of frequency, *1 Hz = 1 / s*.
hertz ∷ DerivedUnit
hertz = makeNonStandard "hertz" "Hz" 1.0 (unity ./ second)

-- | Unit of force, *1 N = 1 kg·m/s²*.
newton ∷ DerivedUnit
newton = makeNonStandard "newton" "N" 1000.0 (gram <> meter ./ second .^ 2.0)

-- | Unit of pressure, *1 Pa = 1 N/m²*.
pascal ∷ DerivedUnit
pascal = makeNonStandard "pascal" "Pa" 1000.0 (gram ./ (meter <> second .^ 2.0))

-- | Unit of energy, *1 J = 1 N·m*.
joule ∷ DerivedUnit
joule = makeNonStandard "joule" "J" 1000.0 (gram <> meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of power, *1 W = 1 J/s*.
watt ∷ DerivedUnit
watt = makeNonStandard "watt" "W" 1000.0 (gram <> meter .^ 2.0 ./ second .^ 3.0)

-- | Unit of electric charge, *1 C = 1 A·s*.
coulomb ∷ DerivedUnit
coulomb = makeNonStandard "coulomb" "C" 1.0 (ampere <> second)

-- | Unit of voltage, *1 V = 1 W/A*.
volt ∷ DerivedUnit
volt = makeNonStandard "volt" "V" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 3.0 <> ampere))

-- | Unit of capacitance, *1 F = 1 C/V*.
farad ∷ DerivedUnit
farad = makeNonStandard "farad" "F" 0.001 (second .^ 4.0 <> ampere .^ 2.0 ./ (gram .^ 1.0 <> meter .^ 2.0))

-- | Unit of electric resistance, *1 Ω = 1 V/A*.
ohm ∷ DerivedUnit
ohm = makeNonStandard "ohm" "Ω" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 3.0 <> ampere .^ 2.0))

-- | Unit of electrical conductance, *1 S = 1 A/V*.
siemens ∷ DerivedUnit
siemens = makeNonStandard "siemens" "S" 0.001 (second .^ 3.0 <> ampere .^ 2.0 ./ (gram <> meter .^ 2.0))

-- | Unit of magnetic flux, *1 Wb = 1 V·s*.
weber ∷ DerivedUnit
weber = makeNonStandard "weber" "Wb" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 2.0 <> ampere))

-- | Unit of magnetic flux density, *1 T = 1 Wb/m²*.
tesla ∷ DerivedUnit
tesla = makeNonStandard "tesla" "T" 1000.0 (gram ./ (second .^ 2.0 <> ampere))

-- | Unit of inductance, *1 H = 1 Wb/A*.
henry ∷ DerivedUnit
henry = makeNonStandard "henry" "H" 1000.0 (gram <> meter .^ 2.0 ./ (second .^ 2.0 <> ampere .^ 2.0))

-- | Unit of luminous flux, *1 lm = 1 cd·sr*.
lumen ∷ DerivedUnit
lumen = makeNonStandard "lumen" "lm" 1.0 candela

-- | Unit of illuminance, *1 lx = 1 lm/m²*.
lux ∷ DerivedUnit
lux = makeNonStandard "lux" "lx" 1.0 (candela ./ meter .^ 2.0)

-- | Unit of radioactivity (decays per time), *1 Bq = 1/s*.
becquerel ∷ DerivedUnit
becquerel = makeNonStandard "becquerel" "Bq" 1.0 (second .^ -1.0)

-- | Unit of absorbed dose, *1 Gy = 1 J/kg*.
gray ∷ DerivedUnit
gray = makeNonStandard "gray" "Gy" 1.0 (meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of equivalent dose, *1 Sv = 1 J/kg*.
sievert ∷ DerivedUnit
sievert = makeNonStandard "sievert" "Sv" 1.0 (meter .^ 2.0 ./ second .^ 2.0)

-- | Unit of catalytic activity, *1 kat = 1 mol/s*.
katal ∷ DerivedUnit
katal = makeNonStandard "katal" "kat" 1.0 (mole ./ second)
