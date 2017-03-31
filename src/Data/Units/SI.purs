-- | This module defines the seven SI base units.
module Data.Units.SI
  ( meter
  , kilogram
  , second
  , gram
  , ampere
  , mole
  , kelvin
  , candela
  ) where

import Data.Units (DerivedUnit, makeStandard, kilo)

-- | The meter is the standard unit of length.
meter ∷ DerivedUnit
meter = makeStandard "meter" "m"

-- | One gram equals one-thousandth of a *kilogram*.
gram ∷ DerivedUnit
gram = makeStandard "gram" "g"

-- | The kilogram is the standard unit of mass.
kilogram ∷ DerivedUnit
kilogram = kilo gram

-- | The second is the standard unit of time.
second ∷ DerivedUnit
second = makeStandard "second" "s"

-- | The ampere is the standard unit of electric current.
ampere ∷ DerivedUnit
ampere = makeStandard "ampere" "A"

-- | The mole is the standard unit for amount of substance.
mole ∷ DerivedUnit
mole = makeStandard "mole" "mol"

-- | The kelvin is the standard unit of temperature.
kelvin ∷ DerivedUnit
kelvin = makeStandard "kelvin" "K"

-- | The candela is the standard unit of luminous intensity.
candela ∷ DerivedUnit
candela = makeStandard "candela" "cd"
