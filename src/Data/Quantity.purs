module Data.Quantity
  ( Quantity()
  , quantity
  , (.*)
  , derivedUnit
  , pow
  , abs
  , toSI
  , approximatelyEqual
  ) where

import Prelude

import Data.Tuple (Tuple(..))

import Data.DerivedUnit (DerivedUnit, toString, (.^))
import Data.DerivedUnit as D

import Math as Math

-- | Representation of a physical quantity as a (product of a) numerical value
-- | and a physical unit.
data Quantity = Quantity Number DerivedUnit

-- Used only internally for pattern matching
infix 3 Quantity as .*.

-- | A smart constructor for physical quantities.
quantity :: Number → DerivedUnit → Quantity
quantity = Quantity -- note that we define `quantity` because we do not want
                    -- to export the `Quantity` constructor. This would leak
                    -- the internal representation and the bare numerical
                    -- values.

infix 3 quantity as .*

instance eqQuantity :: Eq Quantity where
  eq q1 q2 = value q1' == value q2' && derivedUnit q1' == derivedUnit q2'
    where
      q1' = toSI q1
      q2' = toSI q2

instance showQuantity :: Show Quantity where
  show (Quantity num unit) = show num <> toString unit

-- | The numerical value stored inside a `Quantity`. For internal use only
-- | (bare `Number`s without units should be handled with care).
value :: Quantity → Number
value (v .*. _) = v

-- | The unit of a physical quantity.
derivedUnit :: Quantity → DerivedUnit
derivedUnit (_ .*. u) = u

-- | Convert a quantity to its SI representation.
toSI :: Quantity → Quantity
toSI (num .*. du) =
  case D.toSI du of
    Tuple du' conversion -> (conversion * num) .* du'

-- | Raise a quantity to a given power.
pow :: Quantity → Number → Quantity
pow (val .*. u) exp = (val `Math.pow` exp) .* (u .^ exp)

-- | The absolute value of a quantity.
abs :: Quantity → Quantity
abs (val .*. u) = Math.abs val .* u

-- | Check whether two quantities have matching units (or can be converted
-- | to the same representation) and test if the numerical are approximately
-- | equal.
approximatelyEqual :: Number → Quantity → Quantity → Boolean
approximatelyEqual tol q1' q2' =
  derivedUnit q1 == derivedUnit q2 &&
  Math.abs (v1 - v2) < tol * Math.abs (v1 + v2)
    where
      q1 = toSI q1'
      q2 = toSI q2'
      v1 = value q1
      v2 = value q2

{-- data UnificationError = UnificationError DerivedUnit DerivedUnit --}

{-- instance showUnificationError :: Show UnificationError where --}
{--   show (UnificationError u1 u2) = "Cannot unify unit '" <> toString u1 <> --}
{--                                   "' with unit '" <> toString u2 <> "'" --}

{-- convert :: Quantity → DerivedUnit → Either UnificationError Quantity --}
{-- convert q@(v .*. u) u' --}
{--   | u == u' = Right q --}
{--   | otherwise = --}
{--       let q' = toSI q --}

{--       in --}
{--         if qUnit q' == u' --}
{--           then Right q' --}
{--           else Left $ UnificationError u u' --}

{-- qAdd :: Quantity → Quantity → Either UnificationError Quantity --}
{-- qAdd (v1 ⋅ u1) q2 = do --}
{--   q2' <- convert q2 u1 --}
{--   case q2' of --}
{--       (v2 ⋅ _) → pure $ (v1 + v2) ⋅ u1 --}

{-- infixl 6 qAdd as .+. --}

{-- infixl 7 mul as * --}
