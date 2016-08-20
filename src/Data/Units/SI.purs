-- | A collection of standardized SI units.
module Data.Units.SI where

import Data.Units (DerivedUnit, makeStandard)

-- | Standard unit of lenght.
meter :: DerivedUnit
meter = makeStandard "meter" "m"

-- | Standard unit of mass.
gram :: DerivedUnit
gram = makeStandard "gram" "g"

-- | Standard unit of time.
second :: DerivedUnit
second = makeStandard "second" "s"
