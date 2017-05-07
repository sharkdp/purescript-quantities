-- | A collection of units from the cgs system (Gaussian units).
module Data.Units.CGS where

import Prelude ((<>))
import Data.Units (DerivedUnit, makeNonStandard, (./), (.^))
import Data.Units.SI

-- | Unit of magnetic flux density, *1 G = 100 µT*.
gauss ∷ DerivedUnit
gauss = makeNonStandard "gauss" "gauss" 0.1 (gram ./ (second .^ 2.0 <> ampere))
