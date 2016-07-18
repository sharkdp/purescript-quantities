module Data.DerivedUnit
  ( DerivedUnit()
  , power
  , (.^)
  , divideUnits
  , (./)
  , toString
  , toSI
  -- One
  , unity
  -- SI units
  , meter, meters
  , second, seconds
  , gram, grams
  -- Non-standard units
  , minute, minutes
  , hour, hours
  , inch, inches
  ) where

import Prelude hiding (Unit)

import Data.Foldable (intercalate, sum, foldMap, product)
import Data.List (List(Nil), singleton, (:), span, sortBy, filter)
import Data.Monoid (class Monoid)
import Data.NonEmpty (NonEmpty, (:|), head)
import Data.Tuple (Tuple(..), fst, snd)

import Math (pow)

import Data.BaseUnit (shortName, BaseUnit, ConversionFactor, conversionFactor)
import Data.BaseUnit as B

-- | Type alias for something like m^3, s^(-1) or similar
type BaseUnitWithExponent = Tuple BaseUnit Number

baseUnit :: BaseUnitWithExponent → BaseUnit
baseUnit = fst

exponent :: BaseUnitWithExponent → Number
exponent = snd

-- | A `DerivedUnit` is a product of `BaseUnits`, raised to arbitrary powers
data DerivedUnit = DerivedUnit (List BaseUnitWithExponent)

-- | Expose the underlying list of base units
runDerivedUnit :: DerivedUnit → List BaseUnitWithExponent
runDerivedUnit (DerivedUnit u) = u

-- | A `DerivedUnit` corresponding to `1`, i.e. the unit of scalar
-- | (or dimensionless) values.
unity :: DerivedUnit
unity = DerivedUnit Nil

-- | Raise a unit to a given power.
power :: DerivedUnit → Number → DerivedUnit
power u n = DerivedUnit $ map (\(Tuple bu exp) → Tuple bu (exp * n)) (runDerivedUnit u)

infixl 9 power as .^

-- | Divide two given units.
divideUnits :: DerivedUnit → DerivedUnit → DerivedUnit
divideUnits du1 du2 = du1 <> du2 .^ (-1.0)

infixl 6 divideUnits as ./

-- | A `String` representation of a`DerivedUnit`.
toString :: DerivedUnit → String
toString (DerivedUnit us) = intercalate "·" (withExp <$> us)
  where
    withExp (Tuple bu 1.0)   = shortName bu
    withExp (Tuple bu 2.0)   = shortName bu <> "²"
    withExp (Tuple bu 3.0)   = shortName bu <> "³"
    withExp (Tuple bu exp) = shortName bu <> "^" <> show exp

-- | Alternative implementation of `Data.List.groupBy` with a (more) useful
-- | return type.
groupBy :: ∀ a. (a → a → Boolean) → List a → List (NonEmpty List a)
groupBy _ Nil = Nil
groupBy eq (x : xs) = case span (eq x) xs of
  { init: ys, rest: zs } → (x :| ys) : (groupBy eq zs)

-- | Simplify the internal representation of a `DerivedUnit` by merging base
-- | units of the same type. For example, *m·s·m* will by simplified to *m²·s*.
simplify :: DerivedUnit → DerivedUnit
simplify = runDerivedUnit
             >>> sortBy (comparing (baseUnit >>> shortName))
             >>> groupBy (\u1 u2 → baseUnit u1 == baseUnit u2)
             >>> map merge
             >>> filter (\x → exponent x /= 0.0)
             >>> DerivedUnit
  where
    merge units = Tuple (baseUnit (head units)) (sum $ exponent <$> units)

instance eqDerivedUnit :: Eq DerivedUnit where
  eq u1 u2 = runDerivedUnit (simplify u1) == runDerivedUnit (simplify u2)

instance showDerivedUnit :: Show DerivedUnit where
  show = intercalate " <> " <<< map showWithExp <<< runDerivedUnit
    where
      showWithExp (Tuple bu 1.0) = show bu
      showWithExp (Tuple bu   n) = show bu <> " .^ " <> show n

instance semigroupDerivedUnit :: Semigroup DerivedUnit where
  append (DerivedUnit u1) (DerivedUnit u2) = simplify $ DerivedUnit (u1 <> u2)

instance monoidDerivedUnit :: Monoid DerivedUnit where
  mempty = unity

-- | Convert all contained units to SI units and return the global conversion
-- | factor.
toSI :: DerivedUnit → Tuple DerivedUnit ConversionFactor
toSI (DerivedUnit us) = Tuple us' conv
  where
    conv = product $ map (\(Tuple bu exp) → conversionFactor bu `pow` exp) us
    us' = foldMap (\(Tuple bu exp) → du (B.toSI bu) .^ exp) us

-- | Every `BaseUnit` is also a `DerivedUnit`.
du :: BaseUnit → DerivedUnit
du = DerivedUnit <<< singleton <<< (\bu → Tuple bu 1.0)

meter :: DerivedUnit
meter = du B.meter

meters :: DerivedUnit
meters = meter

second :: DerivedUnit
second = du B.second

seconds :: DerivedUnit
seconds = second

gram :: DerivedUnit
gram = du B.gram

grams :: DerivedUnit
grams = gram

minute :: DerivedUnit
minute = du B.minute

minutes :: DerivedUnit
minutes = minute

hour :: DerivedUnit
hour = du B.hour

hours :: DerivedUnit
hours = hour

inch :: DerivedUnit
inch = du B.inch

inches :: DerivedUnit
inches = inch
