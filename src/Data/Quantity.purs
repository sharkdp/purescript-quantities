module Data.Quantity
  ( Quantity()
  , quantity
  , (.*)
  , derivedUnit
  , pow
  , abs
  , toSI
  , approximatelyEqual
  , UnificationError
  , convertTo
  , valueIn
  , qAdd
  , (⊕)
  , qMultiply
  , (⊗)
  ) where

import Prelude

import Data.Either (Either(..))
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

infix 5 quantity as .*

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
    Tuple du' conversion → (conversion * num) .* du'

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

-- | A unit conversion error that appears if two given units cannot be
-- | converted into each other.
data UnificationError = UnificationError DerivedUnit DerivedUnit

derive instance eqUnificationError :: Eq UnificationError

instance showUnificationError :: Show UnificationError where
  show (UnificationError u1 u2) = "(UnificationError " <> show u1 <> " "
                                                       <> show u2 <> ")"

-- | Textual representation of a unit conversion error.
errorMessage :: UnificationError → String
errorMessage (UnificationError u1 u2) =
  "Cannot unify unit '" <> toString u1 <> "' with unit '" <> toString u2 <> "'"

scalar :: Number -> Quantity
scalar factor = factor .* D.unity

-- | Attempt to convert a physical quantity to a given target unit. Returns a
-- | `UnificationError` if the conversion fails.
convertTo :: DerivedUnit → Quantity → Either UnificationError Quantity
convertTo to q@(val .*. from)
  | to == from = Right q
  | otherwise =
      case D.toSI to of
        Tuple to' factor →
          let q' = toSI q
              from' = derivedUnit q'
          in
            if from' == to'
              then Right $ q' ⊗ scalar (1.0 / factor)
              else Left $ UnificationError to from

-- | Get the numerical value of a physical quantity in a given unit. Returns a
-- | `UnificationError` if the conversion fails.
valueIn :: DerivedUnit → Quantity → Either UnificationError Number
valueIn u = convertTo u >=> value >>> pure

-- | Attempt to add two quantities. If the units can not be unified, an error
-- | is returned.
qAdd :: Quantity → Quantity → Either UnificationError Quantity
qAdd (v1 .*. u1) q2 = do
  q2' <- convertTo u1 q2
  case q2' of
    (v2 .*. _) → pure $ (v1 + v2) .* u1

infixl 3 qAdd as ⊕

-- | Multiply two quantities.
qMultiply :: Quantity → Quantity → Quantity
qMultiply (v1 .*. u1) (v2 .*. u2) = (v1 * v2) .* (u1 <> u2)

infixl 4 qMultiply as ⊗
