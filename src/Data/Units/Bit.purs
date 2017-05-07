-- | This module defines a base unit `bit`, and a derived unit `byte`.
module Data.Units.Bit where

import Data.Units (DerivedUnit, makeStandard, makeNonStandard)

-- | Unit of digital information.
bit ∷ DerivedUnit
bit = makeStandard "bit" "bit"

-- | Unit of digital information, *1 byte = 8 bit*.
byte ∷ DerivedUnit
byte = makeNonStandard "byte" "B" 8.0 bit
