-- | A collection of non-SI units that are accepted by the SI brochure.
module Data.Units.SI.Accepted where

import Prelude ((/))

import Math (pi)

import Data.Units (DerivedUnit, makeNonStandard)
import Data.Units.SI.Derived (radian)

-- | 'Dimensionless' unit for angles *360 degree = 2 pi rad*.
degree :: DerivedUnit
degree = makeNonStandard "degree" "°" (pi / 180.0) radian
