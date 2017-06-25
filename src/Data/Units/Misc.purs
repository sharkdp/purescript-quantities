-- | A collection of units that do not fit anywhere else.
module Data.Units.Misc where

import Prelude ((<>), negate)
import Data.Units (DerivedUnit, makeNonStandard, (.^))
import Data.Units.SI

-- | Unit of energy, *1 cal = 4.184 J*.
calorie ∷ DerivedUnit
calorie = makeNonStandard "calorie" "cal" 4184.0 (gram <> meter .^ 2.0 <> second .^ (-2.0))
