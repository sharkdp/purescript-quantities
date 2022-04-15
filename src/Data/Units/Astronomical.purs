-- | A collection of units used in astronomy.
module Data.Units.Astronomical where

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI (meter)

-- | Unit of length, *1 parsec = 3.085677581×10^16 m*.
parsec ∷ DerivedUnit
parsec = makeNonStandard "parsec" "parsec" 3.085677581e16 meter

-- | Unit of length, *1 ly = 9460730472580800 m*.
lightyear ∷ DerivedUnit
lightyear = makeNonStandard "lightyear" "ly" 9460730472580800.0 meter
