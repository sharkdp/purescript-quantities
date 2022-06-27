-- | This module defines data types and functions to handle physical
-- | quantities.
module Data.Quantity
  ( Quantity
  , quantity
  , (.*)
  , quantity'
  , prettyPrint'
  , prettyPrint
  , showResult
  , derivedUnit
  , toStandard
  , fullSimplify
  , approximatelyEqual
  -- Conversion errors
  , ConversionError(..)
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
  -- Numerical properties
  , isFinite
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
import Data.Foldable (product, foldMap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Number.Approximate (Fraction(..), eqRelative)

import Data.Units (DerivedUnit, toString, (.^), (./), unity, removePrefix)
import Data.Units.SI.Derived (radian) as SI
import Data.Units.SI.Accepted (degree) as SI
import Data.Units as U
import Data.Decimal (Decimal, fromNumber, toNumber)
import Data.Decimal as D

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
quantity ∷ Number → DerivedUnit → Quantity
quantity n = Quantity (fromNumber n)

infix 5 quantity as .*

-- | Construct a physical quantity from a numerical value and the physical
-- | unit.
quantity' ∷ Decimal → DerivedUnit → Quantity
quantity' = Quantity

infix 5 quantity' as ..*

instance Eq Quantity where
  eq q1 q2 = (v1 == v2 && u1 == u2) || (v1 == zero && v2 == zero)
    where
      q1' = toStandard q1
      q2' = toStandard q2
      v1 = value q1'
      v2 = value q2'
      u1 = derivedUnit q1'
      u2 = derivedUnit q2'

instance Show Quantity where
  show (Quantity num unit) = show num <> " .* " <> show unit

prettyDecimal ∷ Decimal → String
prettyDecimal d =
  D.toString
    if D.isInteger d && d < D.fromNumber 1.0e18
      then d
      else D.toSignificantDigits 6 d

-- | Show a physical quantity in a human-readable form, value and unit
-- | separately.
prettyPrint' ∷ Quantity → { number ∷ String, space ∷ Boolean, unit ∷ String }
prettyPrint' (val .*. du)
  | du == unity = { number: prettyDecimal val, space: false, unit: "" }
  | otherwise   = { number: prettyDecimal val, space: du /= SI.degree, unit: toString du }

-- | Show a physical quantity in a human-readable form.
prettyPrint ∷ Quantity → String
prettyPrint q =
  if rec.space
    then rec.number <> " " <> rec.unit
    else rec.number <> rec.unit
  where
    rec = prettyPrint' q

-- | Show the (possibly failed) result of a computation in human-readable form.
showResult ∷ Either ConversionError Quantity → String
showResult (Left error) = errorMessage error
showResult (Right q)    = prettyPrint q

-- | The numerical value stored inside a `Quantity`. For internal use only
-- | (bare `Decimal`s without units should be handled with care).
value ∷ Quantity → Decimal
value (v .*. _) = v

-- | The unit of a physical quantity.
derivedUnit ∷ Quantity → DerivedUnit
derivedUnit (_ .*. u) = u

-- | Convert a quantity to its standard representation.
toStandard ∷ Quantity → Quantity
toStandard (num .*. du) =
  case U.toStandardUnit du of
    Tuple du' conversion → (conversion * num) ..* du'

-- | Attempt to simplify the unit of a quantity.
fullSimplify ∷ Quantity → Quantity
fullSimplify q@(num .*. du) =
  case toScalar' q of
    Right n →
     if removePrefix du /= SI.degree && removePrefix du /= SI.radian
       then n .*. unity
       else num ..* du
    Left _ →
      let list = U.splitByDimension du

          toTuple (Tuple target us) =
            case convertTo (one .* us) target of
              Right (f .*. target') → Tuple f target'
              Left _                → Tuple one target

          list' = toTuple <$> list

          factor = product (fst <$> list')
          du' = foldMap snd list'

      in (num * factor) .*. du'

-- | Check whether two quantities have matching units (or can be converted
-- | to the same representation) and test if the numerical values are
-- | approximately equal.
approximatelyEqual ∷ Number → Quantity → Quantity → Boolean
approximatelyEqual tol q1' q2' =
  derivedUnit q1 == derivedUnit q2 &&
  eqRelative (Fraction tol) v1 v2
    where
      q1 = toStandard q1'
      q2 = toStandard q2'
      v1 = toNumber $ value q1
      v2 = toNumber $ value q2

-- | A unit conversion error that appears if two given units cannot be
-- | converted into each other.
data ConversionError = ConversionError DerivedUnit DerivedUnit

derive instance Eq ConversionError

instance Show ConversionError where
  show (ConversionError u1 u2) = "ConversionError (" <> show u1 <> ")"
                                             <> " (" <> show u2 <> ")"

-- | Textual representation of a unit conversion error.
errorMessage ∷ ConversionError → String
errorMessage (ConversionError u1 u2) =
  if u1 == unity
    then "Cannot convert quantity of unit '" <> toString u2 <> "' to a scalar"
    else
      if u2 == unity
        then "Cannot convert quantity of unit '" <> toString u1 <> "' to a scalar"
        else
          "Cannot convert unit '" <> toString u1 <> "'" <> baseRep u1 <> "\n" <>
          "            to unit '" <> toString u2 <> "'" <> baseRep u2 <> ""
  where
    baseRep u =
      let u' = fst (U.toStandardUnit u)
      in
        if u' == unity
          then ""
          else " (SI: '" <> toString u' <> "')"


-- | Create a scalar (i.e. dimensionless) quantity from a number.
scalar ∷ Number → Quantity
scalar factor = factor .* U.unity

-- | Create a scalar (i.e. dimensionless) quantity from a number.
scalar' ∷ Decimal → Quantity
scalar' factor = factor ..* U.unity

-- | Attempt to convert a physical quantity to a given target unit. Returns a
-- | `ConversionError` if the conversion fails.
convert ∷ DerivedUnit → Quantity → Either ConversionError Quantity
convert to q@(val .*. from)
  | to == from  = Right (val .*. to)
  | val == zero = Right (zero .*. to) -- zero can be converted to any unit
  | otherwise   =
      case U.toStandardUnit to of
        Tuple to' factor →
          let q'    = toStandard q
              from' = derivedUnit q'
          in
            if from' == to'
              then Right $ (value q' / factor) .*. to
              else Left $ ConversionError from to

-- | Flipped version of `convert`.
convertTo ∷ Quantity → DerivedUnit → Either ConversionError Quantity
convertTo = flip convert

-- | Get the numerical value of a physical quantity in a given unit. Returns a
-- | `ConversionError` if the conversion fails.
asValueIn' ∷ Quantity → DerivedUnit → Either ConversionError Decimal
asValueIn' u = convertTo u >=> value >>> pure

-- | Get the numerical value of a physical quantity in a given unit. Returns a
-- | `ConversionError` if the conversion fails.
asValueIn ∷ Quantity → DerivedUnit → Either ConversionError Number
asValueIn q u = toNumber <$> q `asValueIn'` u

-- | Try to convert a quantity to a scalar value
toScalar' ∷ Quantity → Either ConversionError Decimal
toScalar' q = q `asValueIn'` unity

-- | Try to convert a quantity to a scalar value
toScalar ∷ Quantity → Either ConversionError Number
toScalar q = q `asValueIn` unity

-- | Check if the numerical value of a quantity is finite.
isFinite ∷ Quantity → Boolean
isFinite (n .*. _) = D.isFinite n

-- | Negate the numerical value of a quantity.
qNegate ∷ Quantity → Quantity
qNegate (v .*. u) = -v .*. u

-- | Attempt to add two quantities. If the units can not be unified, an error
-- | is returned.
qAdd ∷ Quantity → Quantity → Either ConversionError Quantity
qAdd q1@(v1 .*. u1) q2@(v2 .*. _)
  | v1 == zero = pure q2
  | v2 == zero = pure q1
  | otherwise = do
    q2' ← q2 `convertTo` u1
    pure $ (v1 + value q2') ..* u1

infixl 3 qAdd as ⊕

-- | Attempt to subtract two quantities. If the units can not be unified, an
-- | error is returned.
qSubtract ∷ Quantity → Quantity → Either ConversionError Quantity
qSubtract q1 (v2 .*. u2) = q1 ⊕ (-v2 .*. u2)

infixl 3 qSubtract as ⊖

-- | Multiply two quantities.
qMultiply ∷ Quantity → Quantity → Quantity
qMultiply (v1 .*. u1) (v2 .*. u2) = (v1 * v2) ..* (u1 <> u2)

infixl 4 qMultiply as ⊗

-- | Divide two quantities.
qDivide ∷ Quantity → Quantity → Quantity
qDivide (v1 .*. u1) (v2 .*. u2) = (v1 / v2) ..* (u1 ./ u2)

infixl 4 qDivide as ⊘

-- | Raise a quantity to a given power.
pow ∷ Quantity → Decimal → Quantity
pow (val .*. u) exp = (val `D.pow` exp) ..* (u .^ toNumber exp)

-- | The absolute value of a quantity.
abs ∷ Quantity → Quantity
abs (val .*. u) = D.abs val ..* u

-- | The square root of a quantity.
sqrt ∷ Quantity → Quantity
sqrt q = q `pow` (fromNumber 0.5)
