-- | This module defines certain constants of nature.
module Data.Quantity.Physics
  ( speedOfLight
  , gravitationalConstant
  , planckConstant
  , ℏ
  , electronMass
  , electronCharge
  , µ0
  , ε0
  , µB
  , α
  , protonMass
  , avogadroConstant
  , kB
  , g0
  , idealGasConstant
  , faradayConstant
  ) where

import Prelude

import Data.Decimal (fromInt)
import Data.Either (Either(..))
import Data.Quantity ((⊗), (⊘), (.*), Quantity, scalar, pow, fullSimplify, convertTo)
import Data.Quantity.Math (pi)
import Data.Units ((.^), (./))
import Data.Units.SI (ampere, kelvin, kilogram, meter, mole, second)
import Data.Units.SI.Derived (joule, coulomb, newton, tesla, farad)

-- | The speed of light in vacuum.
speedOfLight ∷ Quantity
speedOfLight = 299792458.0 .* meter ./ second

-- | The Newtonian constant of gravitation.
gravitationalConstant ∷ Quantity
gravitationalConstant = 6.67408e-11 .* meter .^ 3.0 ./ (kilogram <> second .^ 2.0)

-- | The Planck constant.
planckConstant ∷ Quantity
planckConstant = 6.626070040e-34 .* (joule <> second)

-- | The reduced Planck constant.
ℏ ∷ Quantity
ℏ = planckConstant ⊘ (scalar 2.0 ⊗ pi)

-- | The mass of the electron.
electronMass ∷ Quantity
electronMass = 9.1093826e-31 .* kilogram

-- | Elementary charge (charge of the electron).
electronCharge ∷ Quantity
electronCharge = 1.60217653e-19 .* coulomb

-- | Magnetic constant (vacuum permeability).
µ0 ∷ Quantity
µ0 = pi ⊗ (4.0e-7 .* newton ./ ampere .^ 2.0)

-- | Electric constant (vacuum permittivity).
ε0 ∷ Quantity
ε0 = case ε0' `convertTo` (farad ./ meter) of
       Right q → q
       Left _ → ε0'
  where
    ε0' = scalar 1.0 ⊘ (µ0 ⊗ (speedOfLight `pow` (fromInt 2)))

-- | Bohr magneton.
µB ∷ Quantity
µB = 9.274009994e-24 .* (joule ./ tesla)

-- | Fine structure constant.
α ∷ Quantity
α = fullSimplify $ electronCharge `pow` (fromInt 2) ⊘
      (scalar 4.0 ⊗ pi ⊗ ε0 ⊗ ℏ ⊗ speedOfLight)

-- | Mass of the proton.
protonMass ∷ Quantity
protonMass = 1.672621898e-27 .* kilogram

-- | Avogadro's number.
avogadroConstant ∷ Quantity
avogadroConstant = 6.022140857e23 .* mole .^ -1.0

-- | Boltzmann constant.
kB ∷ Quantity
kB = 1.38064852e-23 .* (joule ./ kelvin)

-- | Standard gravitational acceleration on earth.
g0 ∷ Quantity
g0 = 9.80665 .* meter ./ second .^ (2.0)

-- | Ideal gas constant.
idealGasConstant ∷ Quantity
idealGasConstant = 8.3145 .* joule ./ (mole <> kelvin)

-- | Faraday constant.
faradayConstant ∷ Quantity
faradayConstant = electronCharge ⊗ avogadroConstant
