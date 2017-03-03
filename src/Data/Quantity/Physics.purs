module Data.Quantity.Physics
  ( speedOfLight
  , gravitationalConstant
  , planckConstant
  , hbar
  ) where

import Prelude

import Data.Quantity ((⊗), (⊘), (.*), Quantity, UnificationError, asValueIn,
                      scalar)
import Data.Quantity.Math (pi)
import Data.Units ((.^), (./), unity, kilo)
import Data.Units.SI (meter, second, gram)
import Data.Units.SI.Derived (joule)

-- | The speed of light in vacuum.
speedOfLight ∷ Quantity
speedOfLight = 299792458.0 .* meter ./ second

-- | The Newtonian constant of gravitation.
gravitationalConstant ∷ Quantity
gravitationalConstant = 6.67408e-11 .* meter .^ 3.0 ./ (kilo gram <> second .^ 2.0)

-- | The Planck constant.
planckConstant ∷ Quantity
planckConstant = 6.626070040e-34 .* (joule <> second)

-- | The reduced Planck constant.
hbar ∷ Quantity
hbar = planckConstant ⊘ (scalar 2.0 ⊗ pi)
