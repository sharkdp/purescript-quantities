-- | A collection of units that do not fit anywhere else.
module Data.Units.Misc where

import Prelude ((<>), negate, (/), (*))
import Data.Units (DerivedUnit, makeStandard, makeNonStandard, (.^), (./))
import Data.Units.SI

-- | Unit of energy, *1 cal = 4.184 J*.
calorie ∷ DerivedUnit
calorie = makeNonStandard "calorie" "cal" 4184.0 (gram <> meter .^ 2.0 <> second .^ -2.0)

-- | Unit of energy, *1 BTU = 1055.05585262 J*.
btu ∷ DerivedUnit
btu = makeNonStandard "BTU" "BTU" 1055055.85262 (gram <> meter .^ 2.0 <> second .^ -2.0)

-- | Unit of force, *1 lbf = 4.448222 N*.
lbf ∷ DerivedUnit
lbf = makeNonStandard "pound_force" "lbf" 4448.222 (gram <> meter <> second .^ -2.0)

-- | Unit of force, *1 ozf = 16 lbf*.
ozf ∷ DerivedUnit
ozf = makeNonStandard "ounce_force" "ozf" (4448.222 / 16.0) (gram <> meter <> second .^ -2.0)

-- | Unit of force, *1 kgf = 9.806650 N*.
kgf ∷ DerivedUnit
kgf = makeNonStandard "pound_force" "kgf" 9806.650 (gram <> meter <> second .^ -2.0)

-- | Unit of frequency, *1 rpm = 1/min*.
rpm ∷ DerivedUnit
rpm = makeNonStandard "rpm" "rpm" (1.0 / 60.0) (second .^ -1.0)

-- | Unit of time, *1 fortnight = 2 weeks*.
fortnight ∷ DerivedUnit
fortnight = makeNonStandard "fortnight" "fortnight" (14.0 * 24.0 * 3600.0) second

-- | Unit of pressure, *1 mmHg = 133.322387415 Pa*.
mmHg ∷ DerivedUnit
mmHg = makeNonStandard "mmHg" "mmHg" 133322.387415 (gram ./ (meter <> second .^ 2.0))

-- | Unit of pressure, *1 psi = 6.894757 kPa*.
psi ∷ DerivedUnit
psi = makeNonStandard "psi" "psi" 6894757.0 (gram ./ (meter <> second .^ 2.0))

-- | Unit of pressure, *1 atm = 101325 Pa*.
atm ∷ DerivedUnit
atm = makeNonStandard "atm" "atm" 101325000.0 (gram ./ (meter <> second .^ 2.0))

-- | Unit of molar concentration, *1 molar = 1 mol/L*.
molar ∷ DerivedUnit
molar = makeNonStandard "molar" "molar" 1000.0 (mole ./ meter .^ 3.0)

-- | Unit of molal concentration, *1 molal = 1 mol/kg*.
molal ∷ DerivedUnit
molal = makeNonStandard "molal" "molal" 0.001 (mole ./ gram)

-- | Smallest addressable element on a digital display.
pixel ∷ DerivedUnit
pixel = makeStandard "pixel" "px"

-- | Smallest possible output resolution on a printing device.
dot ∷ DerivedUnit
dot = makeStandard "dot" "dot"

-- | A single image in a (video) sequence.
frame ∷ DerivedUnit
frame = makeStandard "frame" "frame"

-- | A separate or limited portion or quantity of something.
piece ∷ DerivedUnit
piece = makeStandard "piece" "piece"

-- | A human being.
person ∷ DerivedUnit
person = makeStandard "person" "person"
