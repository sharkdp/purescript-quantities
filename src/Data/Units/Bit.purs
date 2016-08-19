module Data.Units.Bit where

import Data.Units (DerivedUnit, makeStandard, makeNonStandard)

bit :: DerivedUnit
bit = makeStandard "bit" "bit"

byte :: DerivedUnit
byte = makeNonStandard "byte" "b" bit 8.0
