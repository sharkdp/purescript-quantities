module Data.Quantity
  ( Quantity
  , quantity
  , (.*)
  , quantity'
  , prettyPrint
  , showResult
  , derivedUnit
  , toStandard
  , fullSimplify
  , approximatelyEqual
  -- Conversion errors
  , UnificationError
  , errorMessage
  -- Create a dimensionless quantity
  , scalar
  , scalar'
  -- Convert quantities
  , convert
  , convertTo
  , asValueIn
  , asValueIn'
  , toScalar
  , toScalar'
  -- Calculate with quantities
  , qNegate
  , qAdd
  , (⊕)
  , qSubtract
  , (⊖)
  , qMultiply
  , (⊗)
  , qDivide
  , (⊘)
  , pow
  , abs
  , sqrt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Number (eqRelative)

import Data.Units (DerivedUnit, toString, toStringWithPrefix, (.^), (./), unity)
import Data.Units.SI.Accepted as USIA
import Data.Units as U
import Data.Decimal (Decimal, fromNumber, toNumber)
import Data.Decimal as Decimal

-- | Representation of a physical quantity as a (product of a) numerical value
-- | and a physical unit.
data Quantity = Quantity Decimal DerivedUnit

-- | Helper operator, used internally for pattern matching.
infix 3 Quantity as .*.

-- Note that we define `quantity` below because we do not want to export the
-- `Quantity` constructor. This would leak the internal representation and the
-- bare numerical values.

-- | Construct a physical quantity from a numerical value and the physical
-- | unit.
quantity :: Number → DerivedUnit → Quantity
quantity n du = Quantity (fromNumber n) du

infix 5 quantity as .*

-- | Construct a physical quantity from a numerical value and the physical
-- | unit.
quantity' :: Decimal → DerivedUnit → Quantity
quantity' n du = Quantity n du

infix 5 quantity' as ..*

instance eqQuantity :: Eq Quantity where
  eq q1 q2 = value q1' == value q2' && derivedUnit q1' == derivedUnit q2'
    where
      q1' = toStandard q1
      q2' = toStandard q2

instance showQuantity :: Show Quantity where
  show (Quantity num unit) = show num <> " .* " <> show unit

prettyDecimal :: Decimal → String
prettyDecimal d =
  if Decimal.isInteger d && d < (Decimal.fromNumber 1.0e18)
    then Decimal.toString d
    else Decimal.toString (Decimal.toSignificantDigits 6 d)

-- | Show a physical quantity in a human-readable form.
prettyPrint :: Quantity → String
prettyPrint (val .*. du)
  | du == unity = prettyDecimal val
  | otherwise   = let res = toStringWithPrefix du
                  in prettyDecimal val <> res.prefix <> res.value

-- | Show the (possibly failed) result of a computation in human-readable form.
showResult :: Either UnificationError Quantity → String
showResult (Left error) = errorMessage error
showResult (Right q)    = prettyPrint q

-- | The numerical value stored inside a `Quantity`. For internal use only
-- | (bare `Decimal`s without units should be handled with care).
value :: Quantity → Decimal
value (v .*. _) = v

-- | The unit of a physical quantity.
derivedUnit :: Quantity → DerivedUnit
derivedUnit (_ .*. u) = u

-- | Convert a quantity to its standard representation.
toStandard :: Quantity → Quantity
toStandard (num .*. du) =
  case U.toStandardUnit du of
    Tuple du' conversion → (fromNumber conversion * num) ..* du'

-- | Simplify the unit of a quantity.
fullSimplify :: Quantity → Quantity
fullSimplify q@(num .*. du) =
  case toScalar' q of
    Left _ → num .*. U.simplify du
    Right n → if du /= USIA.degree
                then n .*. unity
                else num ..* du

-- | Check whether two quantities have matching units (or can be converted
-- | to the same representation) and test if the numerical are approximately
-- | equal.
approximatelyEqual :: Number → Quantity → Quantity → Boolean
approximatelyEqual tol q1' q2' =
  derivedUnit q1 == derivedUnit q2 &&
  eqRelative tol v1 v2
    where
      q1 = toStandard q1'
      q2 = toStandard q2'
      v1 = toNumber $ value q1
      v2 = toNumber $ value q2

-- | A unit conversion error that appears if two given units cannot be
-- | converted into each other.
data UnificationError = UnificationError DerivedUnit DerivedUnit

derive instance eqUnificationError :: Eq UnificationError

instance showUnificationError :: Show UnificationError where
  show (UnificationError u1 u2) = "UnificationError (" <> show u1 <> ")"
                                               <> " (" <> show u2 <> ")"

-- | Textual representation of a unit conversion error.
errorMessage :: UnificationError → String
errorMessage (UnificationError u1 u2) =
  if u1 == unity
    then "Cannot convert quantity of unit '" <> toString u2 <> "' to a scalar"
    else
      if u2 == unity
        then "Cannot convert quantity of unit '" <> toString u1 <> "' to a scalar"
        else "Cannot unify unit '" <> toString u1 <> "' with unit '" <> toString u2 <> "'"

-- | Create a scalar (i.e. dimensionless) quantity from a number.
scalar :: Number → Quantity
scalar factor = factor .* U.unity

-- | Create a scalar (i.e. dimensionless) quantity from a number.
scalar' :: Decimal → Quantity
scalar' factor = factor ..* U.unity

-- | Attempt to convert a physical quantity to a given target unit. Returns a
-- | `UnificationError` if the conversion fails.
convert :: DerivedUnit → Quantity → Either UnificationError Quantity
convert to q@(val .*. from)
  | to == from = Right q
  | otherwise =
      case U.toStandardUnit to of
        Tuple to' factor →
          let q'    = toStandard q
              from' = derivedUnit q'
          in
            if from' == to'
              then Right $ case q' of
                             (val' .*. _) → (val' / fromNumber factor) .*. to
              else Left $ UnificationError from to

-- | Flipped version of `convert`.
convertTo :: Quantity → DerivedUnit → Either UnificationError Quantity
convertTo = flip convert

-- | Get the numerical value of a physical quantity in a given unit. Returns a
-- | `UnificationError` if the conversion fails.
asValueIn' :: Quantity → DerivedUnit → Either UnificationError Decimal
asValueIn' u = convertTo u >=> value >>> pure

-- | Get the numerical value of a physical quantity in a given unit. Returns a
-- | `UnificationError` if the conversion fails.
asValueIn :: Quantity → DerivedUnit → Either UnificationError Number
asValueIn q u = toNumber <$> (asValueIn' q u)

-- | Try to convert a quantity to a scalar value
toScalar' :: Quantity → Either UnificationError Decimal
toScalar' q = q `asValueIn'` unity

-- | Try to convert a quantity to a scalar value
toScalar :: Quantity → Either UnificationError Number
toScalar q = q `asValueIn` unity

-- | Negate the numerical value of a quantity.
qNegate :: Quantity → Quantity
qNegate (v .*. u) = (-v) .*. u

-- | Attempt to add two quantities. If the units can not be unified, an error
-- | is returned.
qAdd :: Quantity → Quantity → Either UnificationError Quantity
qAdd (v1 .*. u1) q2 = do
  q2' <- q2 `convertTo` u1
  case q2' of
    (v2 .*. _) → pure $ (v1 + v2) ..* u1

infixl 3 qAdd as ⊕

-- | Attempt to subtract two quantities. If the units can not be unified, an
-- | error is returned.
qSubtract :: Quantity → Quantity → Either UnificationError Quantity
qSubtract q1 (v2 .*. u2) = q1 ⊕ ((-v2) .*. u2)

infixl 3 qSubtract as ⊖

-- | Multiply two quantities.
qMultiply :: Quantity → Quantity → Quantity
qMultiply (v1 .*. u1) (v2 .*. u2) = (v1 * v2) ..* (u1 <> u2)

infixl 4 qMultiply as ⊗

-- | Divide two quantities.
qDivide :: Quantity → Quantity → Quantity
qDivide (v1 .*. u1) (v2 .*. u2) = (v1 / v2) ..* (u1 ./ u2)

infixl 4 qDivide as ⊘

-- | Raise a quantity to a given power.
pow :: Quantity → Decimal → Quantity
pow (val .*. u) exp = (val `Decimal.pow` exp) ..* (u .^ toNumber exp)

-- | The absolute value of a quantity.
abs :: Quantity → Quantity
abs (val .*. u) = Decimal.abs val ..* u

-- | The square root of a quantity.
sqrt :: Quantity → Quantity
sqrt q = q `pow` (fromNumber 0.5)
