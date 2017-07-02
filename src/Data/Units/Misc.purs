-- | A collection of units that do not fit anywhere else.
module Data.Units.Misc where

import Prelude ((<>), negate, (/), (*))
import Data.Units (DerivedUnit, makeNonStandard, (.^), (./))
import Data.Units.SI

-- | Unit of energy, *1 cal = 4.184 J*.
calorie ∷ DerivedUnit
calorie = makeNonStandard "calorie" "cal" 4184.0 (gram <> meter .^ 2.0 <> second .^ (-2.0))

-- | Unit of frequency, *1 rpm = 1/min*.
rpm ∷ DerivedUnit
rpm = makeNonStandard "rpm" "rpm" (1.0 / 60.0) (second .^ (-1.0))

-- | Unit of time, *1 fortnight = 2 weeks*.
fortnight ∷ DerivedUnit
fortnight = makeNonStandard "fortnight" "fortnight" (14.0 * 24.0 * 3600.0) second

-- | Unit of pressure, *1 mmHg = 133.322387415 Pa*.
mmHg ∷ DerivedUnit
mmHg = makeNonStandard "mmHg" "mmHg" (133322.387415) (gram ./ (meter <> second .^ 2.0))

-- | Unit of pressure, *1 psi = 6.894757 kPa*.
psi ∷ DerivedUnit
psi = makeNonStandard "psi" "psi" (6894757.0) (gram ./ (meter <> second .^ 2.0))

-- | Unit of pressure, *1 atm = 101325 Pa*.
atm ∷ DerivedUnit
atm = makeNonStandard "atm" "atm" (101325000.0) (gram ./ (meter <> second .^ 2.0))
