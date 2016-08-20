module Data.Units
  ( DerivedUnit()
  , makeStandard
  , makeNonStandard
  -- Conversions
  , toStandardUnit
  , toString
  -- Mathematical operations on units
  , power
  , (.^)
  , divideUnits
  , (./)
  -- One
  , unity
  -- Prefixes
  , atto
  , femto
  , pico
  , nano
  , micro
  , centi
  , deci
  , hecto
  , milli
  , kilo
  , mega
  , giga
  , tera
  , peta
  , exa
  ) where

import Prelude

import Data.Foldable (intercalate, sum, foldMap, product)
import Data.List (List(Nil), singleton, (:), span, sortBy, filter)
import Data.Monoid (class Monoid)
import Data.NonEmpty (NonEmpty, (:|), head)
import Data.Tuple (Tuple(..), fst, snd)

import Math (pow)


type ConversionFactor = Number

-- | A base unit can either be a standardized unit or some non-standard unit.
-- | In the latter case, a conversion to a standard unit must be provided.
data UnitType
  = Standard
  | NonStandard
      { standardUnit :: DerivedUnit
      , factor       :: ConversionFactor
      }

instance eqUnitType :: Eq UnitType where
  eq Standard Standard = true
  eq (NonStandard rec1) (NonStandard rec2) = rec1.standardUnit == rec2.standardUnit
                                          &&       rec1.factor == rec2.factor
  eq _ _ = false

-- | A (single) physical unit like *meter* or *second*.
newtype BaseUnit = BaseUnit
  { long     :: String
  , short    :: String
  , unitType :: UnitType
  }

-- | The short name of a base unit (m, s, ..).
shortName :: BaseUnit → String
shortName (BaseUnit u) = u.short

-- | The long name of a base unit (meter, second, ..).
longName :: BaseUnit → String
longName (BaseUnit u) = u.long

instance eqBaseUnit :: Eq BaseUnit where
  eq (BaseUnit u1) (BaseUnit u2) =     u1.long == u2.long
                                &&    u1.short == u2.short
                                && u1.unitType == u2.unitType

instance showBaseUnit :: Show BaseUnit where
  show = longName

-- | Test whether or not a given `BaseUnit` is a standard unit.
isStandardUnit :: BaseUnit → Boolean
isStandardUnit (BaseUnit u) =
  case u.unitType of
    Standard → true
    _        → false

-- | Convert a base unit to a standard unit.
baseToStandard :: BaseUnit → DerivedUnit
baseToStandard bu@(BaseUnit u) =
  case u.unitType of
      Standard → fromBaseUnit bu
      NonStandard { standardUnit, factor } → standardUnit

conversionFactor :: BaseUnit → ConversionFactor
conversionFactor (BaseUnit u) =
  case u.unitType of
      Standard → 1.0
      NonStandard { standardUnit, factor } → factor




-- | Type alias for something like m^3, s^(-1) or similar
type BaseUnitWithExponent = { baseUnit :: BaseUnit
                            , exponent :: Number }

type Prefix = Number

-- | A `DerivedUnit` is a product of `BaseUnits`, raised to arbitrary powers.
-- | The `Semigroup`/`Monoid` instance implements multiplication of units. A
-- | `DerivedUnit` also has a `Prefix` value, which represents a numerical
-- | prefix as a power of ten.
data DerivedUnit = DerivedUnit Prefix (List BaseUnitWithExponent)

-- | Expose the underlying list of base units.
runDerivedUnit :: DerivedUnit → List BaseUnitWithExponent
runDerivedUnit (DerivedUnit _ u) = u

-- | Expose the underlying list of base units.
prefix :: DerivedUnit → Prefix
prefix (DerivedUnit p _) = p

-- | Alternative implementation of `Data.List.groupBy` with a (more) useful
-- | return type.
groupBy :: ∀ a. (a → a → Boolean) → List a → List (NonEmpty List a)
groupBy _ Nil = Nil
groupBy eq (x : xs) = case span (eq x) xs of
  { init: ys, rest: zs } → (x :| ys) : groupBy eq zs

-- | Simplify the internal representation of a `DerivedUnit` by merging base
-- | units of the same type. For example, *m·s·m* will by simplified to *m²·s*.
simplify :: DerivedUnit → DerivedUnit
simplify (DerivedUnit p list) = DerivedUnit p (go list)
  where
    go = sortBy (comparing (_.baseUnit >>> shortName))
           >>> groupBy (\u1 u2 → u1.baseUnit == u2.baseUnit)
           >>> map merge
           >>> filter (\x → x.exponent /= 0.0)
    merge units = { baseUnit: (head units).baseUnit
                  , exponent: sum $ _.exponent <$> units }

instance eqDerivedUnit :: Eq DerivedUnit where
  eq u1 u2 = (_.baseUnit <$> list1 == _.baseUnit <$> list2)
          && (_.exponent <$> list1 == _.exponent <$> list2)
          &&             prefix u1 == prefix u2
    where
      list1 = runDerivedUnit (simplify u1)
      list2 = runDerivedUnit (simplify u2)

instance showDerivedUnit :: Show DerivedUnit where
  show du = showList (runDerivedUnit du)
    where
      showList Nil | prefix' == 0.0 = "1.0"
                   | otherwise      = prefixStr
      showList xs  | prefix' == 0.0 = listString xs
                   | otherwise      = prefixStr <> " .* " <> listString xs
      listString xs = intercalate " <> " (showWithExp <$> xs)
      showWithExp { baseUnit, exponent: 1.0 } = show baseUnit
      showWithExp { baseUnit, exponent }      = show baseUnit
                                                  <> " .^ " <> show exponent
      prefix' = prefix du
      prefixStr = if prefix' == 0.0 then "" else show (10.0 `pow` prefix')

instance semigroupDerivedUnit :: Semigroup DerivedUnit where
  append (DerivedUnit p1 u1) (DerivedUnit p2 u2) =
    simplify $ DerivedUnit (p1 + p2) (u1 <> u2)

instance monoidDerivedUnit :: Monoid DerivedUnit where
  mempty = unity

-- | Helper function to create a standard unit.
makeStandard :: String → String → DerivedUnit
makeStandard long short = fromBaseUnit $
  BaseUnit { short, long, unitType: Standard }

-- | Helper function to create a non-standard unit.
makeNonStandard :: String → String → ConversionFactor → DerivedUnit
                   → DerivedUnit
makeNonStandard long short factor standardUnit = fromBaseUnit $
  BaseUnit { short, long, unitType: NonStandard { standardUnit, factor } }

-- | Convert all contained units to standard units and return the global
-- | conversion factor.
toStandardUnit :: DerivedUnit → Tuple DerivedUnit ConversionFactor
toStandardUnit (DerivedUnit prf units) = Tuple units' conv
  where
    conv = 10.0 `pow` prf * product (snd <$> converted)
    units' = foldMap fst converted

    converted = convert <$> units

    convert :: BaseUnitWithExponent → Tuple DerivedUnit Number
    convert { baseUnit, exponent } =
      Tuple ((baseToStandard baseUnit) .^ exponent)
            (conversionFactor baseUnit `pow` exponent)

-- | Convert a prefixed exponent to a `String`.
prefixToString :: Prefix → String
prefixToString -18.0 = "a"
prefixToString -15.0 = "f"
prefixToString -12.0 = "p"
prefixToString  -9.0 = "n"
prefixToString  -6.0 = "µ"
prefixToString  -3.0 = "m"
prefixToString  -2.0 = "c"
prefixToString  -1.0 = "d"
prefixToString   0.0 = ""
prefixToString   2.0 = "h"
prefixToString   3.0 = "k"
prefixToString   6.0 = "M"
prefixToString   9.0 = "G"
prefixToString  12.0 = "T"
prefixToString  15.0 = "P"
prefixToString  18.0 = "E"
prefixToString     p = "10^(" <> show p <> ")·"

-- | A `String` representation of a `DerivedUnit`.
toString :: DerivedUnit → String
toString (DerivedUnit prf us) = prefixToString prf <> intercalate "·" (withExp <$> us)
  where
    withExp { baseUnit, exponent: 1.0 } = shortName baseUnit
    withExp { baseUnit, exponent: 2.0 } = shortName baseUnit <> "²"
    withExp { baseUnit, exponent: 3.0 } = shortName baseUnit <> "³"
    withExp { baseUnit, exponent } = shortName baseUnit <> "^(" <> show exponent <> ")"

-- | Raise a unit to the given power.
power :: DerivedUnit → Number → DerivedUnit
power u n = DerivedUnit (prefix u * n) $ map update (runDerivedUnit u)
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
unity = DerivedUnit 0.0 Nil

-- | Convert a `BaseUnit` to a `DerivedUnit`.
fromBaseUnit :: BaseUnit → DerivedUnit
fromBaseUnit = DerivedUnit 0.0 <<< singleton <<< (\bu → { baseUnit: bu, exponent: 1.0 })

siPrefix :: Number → DerivedUnit → DerivedUnit
siPrefix exp (DerivedUnit p us) = DerivedUnit (p + exp) us

atto :: DerivedUnit → DerivedUnit
atto = siPrefix (-18.0)

femto :: DerivedUnit → DerivedUnit
femto = siPrefix (-15.0)

pico :: DerivedUnit → DerivedUnit
pico = siPrefix (-12.0)

nano :: DerivedUnit → DerivedUnit
nano = siPrefix (-9.0)

micro :: DerivedUnit → DerivedUnit
micro = siPrefix (-6.0)

milli :: DerivedUnit → DerivedUnit
milli = siPrefix (-3.0)

centi :: DerivedUnit → DerivedUnit
centi = siPrefix (-2.0)

deci :: DerivedUnit → DerivedUnit
deci = siPrefix (-1.0)

hecto :: DerivedUnit → DerivedUnit
hecto = siPrefix 2.0

kilo :: DerivedUnit → DerivedUnit
kilo = siPrefix 3.0

mega :: DerivedUnit → DerivedUnit
mega = siPrefix 6.0

giga :: DerivedUnit → DerivedUnit
giga = siPrefix 9.0

tera :: DerivedUnit → DerivedUnit
tera = siPrefix 12.0

peta :: DerivedUnit → DerivedUnit
peta = siPrefix 15.0

exa :: DerivedUnit → DerivedUnit
exa = siPrefix 18.0
