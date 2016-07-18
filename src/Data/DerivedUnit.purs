module Data.DerivedUnit
  ( DerivedUnit()
  , toStandardUnit
  , toString
  , power
  , (.^)
  , divideUnits
  , (./)
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
  , foot, feet
  , mile, miles
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
type BaseUnitWithExponent = { baseUnit :: BaseUnit
                            , exponent :: Number }

-- | A `DerivedUnit` is a product of `BaseUnits`, raised to arbitrary powers.
-- | The `Semigroup`/`Monoid` instance implements multiplication of units.
data DerivedUnit = DerivedUnit (List BaseUnitWithExponent)

-- | Expose the underlying list of base units.
runDerivedUnit :: DerivedUnit → List BaseUnitWithExponent
runDerivedUnit (DerivedUnit u) = u

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
             >>> sortBy (comparing (_.baseUnit >>> shortName))
             >>> groupBy (\u1 u2 → u1.baseUnit == u2.baseUnit)
             >>> map merge
             >>> filter (\x → x.exponent /= 0.0)
             >>> DerivedUnit
  where
    merge units = { baseUnit: (head units).baseUnit
                  , exponent: sum $ _.exponent <$> units }

instance eqDerivedUnit :: Eq DerivedUnit where
  eq u1 u2 = (_.baseUnit <$> list1 == _.baseUnit <$> list2)
          && (_.exponent <$> list1 == _.exponent <$> list2)
    where
      list1 = runDerivedUnit (simplify u1)
      list2 = runDerivedUnit (simplify u2)

instance showDerivedUnit :: Show DerivedUnit where
  show = intercalate " <> " <<< map showWithExp <<< runDerivedUnit
    where
      showWithExp { baseUnit, exponent: 1.0 } = show baseUnit
      showWithExp { baseUnit, exponent } = show baseUnit <> " .^ "
                                                         <> show exponent

instance semigroupDerivedUnit :: Semigroup DerivedUnit where
  append (DerivedUnit u1) (DerivedUnit u2) = simplify $ DerivedUnit (u1 <> u2)

instance monoidDerivedUnit :: Monoid DerivedUnit where
  mempty = unity

-- | Convert all contained units to standard SI units and return the global
-- | conversion factor.
toStandardUnit :: DerivedUnit → Tuple DerivedUnit ConversionFactor
toStandardUnit (DerivedUnit units) = Tuple units' conv
  where
    conv = product $ snd <$> converted
    units' = foldMap fst converted

    converted = convert <$> units

    convert :: BaseUnitWithExponent → Tuple DerivedUnit Number
    convert { baseUnit, exponent } =
      Tuple (du (B.toStandardUnit baseUnit) .^ exponent)
            (conversionFactor baseUnit `pow` exponent)

-- | A `String` representation of a `DerivedUnit`.
toString :: DerivedUnit → String
toString (DerivedUnit us) = intercalate "·" (withExp <$> us)
  where
    withExp { baseUnit, exponent: 1.0 } = shortName baseUnit
    withExp { baseUnit, exponent: 2.0 } = shortName baseUnit <> "²"
    withExp { baseUnit, exponent: 3.0 } = shortName baseUnit <> "³"
    withExp { baseUnit, exponent } = shortName baseUnit <> "^(" <> show exponent <> ")"

-- | Raise a unit to the given power.
power :: DerivedUnit → Number → DerivedUnit
power u n = DerivedUnit $ map update (runDerivedUnit u)
  where
    update rec = rec { exponent = rec.exponent * n }

infixl 9 power as .^

-- | Divide two units.
divideUnits :: DerivedUnit → DerivedUnit → DerivedUnit
divideUnits du1 du2 = du1 <> du2 .^ (-1.0)

infixl 6 divideUnits as ./

-- | A `DerivedUnit` corresponding to `1`, i.e. the unit of scalar
-- | (or dimensionless) values.
unity :: DerivedUnit
unity = DerivedUnit Nil

-- | Every `BaseUnit` is also a `DerivedUnit`.
du :: BaseUnit → DerivedUnit
du = DerivedUnit <<< singleton <<< (\bu → { baseUnit: bu, exponent: 1.0 })

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

foot :: DerivedUnit
foot = du B.foot

feet :: DerivedUnit
feet = foot

mile :: DerivedUnit
mile = du B.mile

miles :: DerivedUnit
miles = mile
