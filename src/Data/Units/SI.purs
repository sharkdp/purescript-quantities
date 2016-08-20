-- | A collection of standardized SI units.
module Data.Units.SI
  ( meter
  , kilogram
  , second
  , gram
  ) where

import Data.Units (DerivedUnit, makeStandard, kilo)

-- | The meter is the standard unit of length.
meter :: DerivedUnit
meter = makeStandard "meter" "m"

-- | The kilogram is the standard unit of mass.
kilogram :: DerivedUnit
kilogram = kilo gram

-- | The second is the standard unit of time.
second :: DerivedUnit
second = makeStandard "second" "s"

-- | One gram equals one-thousandth of a *kilogram*.
gram :: DerivedUnit
gram = makeStandard "gram" "g"
