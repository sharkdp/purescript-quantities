-- | This module defines data types and functions to handle physical units.
module Data.Units
  ( Prefix
  , DerivedUnit()
  , withPrefix
  , removePrefix
  , simplify
  , splitByDimension
  , makeStandard
  , makeNonStandard
  -- Conversions
  , toStandardUnit
  , prefixName
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

import Data.Decimal (Decimal, pow, fromNumber, toNumber)
import Data.Foldable (intercalate, sum, foldMap, product)
import Data.Function (on)
import Data.List (List(Nil), filter, findIndex, modifyAt,
                  singleton, sortBy, span, (:), concat)
import Data.List.NonEmpty (NonEmptyList(..), head, toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)

-- | A factor which is used to convert between two units. For the conversion
-- | from `minute` to `second`, the conversion factor would be `60.0`.
type ConversionFactor = Decimal

-- | A base unit can either be a standardized unit or some non-standard unit.
-- | In the latter case, a conversion to a standard unit must be provided.
data UnitType
  = Standard
  | NonStandard
      { standardUnit ∷ DerivedUnit
      , factor       ∷ ConversionFactor
      }

instance eqUnitType ∷ Eq UnitType where
  eq Standard Standard = true
  eq (NonStandard rec1) (NonStandard rec2) = rec1.standardUnit == rec2.standardUnit
                                          &&       rec1.factor == rec2.factor
  eq _ _ = false

-- | A (single) physical unit, for example *meter* or *second*.
newtype BaseUnit = BaseUnit
  { long     ∷ String
  , short    ∷ String
  , unitType ∷ UnitType
  }

-- | The short name of a base unit (*meter* → *m*, *second* → *s*, ..).
shortName ∷ BaseUnit → String
shortName (BaseUnit u) = u.short

-- | The long name of a base unit (*meter*, *second*, ..).
longName ∷ BaseUnit → String
longName (BaseUnit u) = u.long

instance eqBaseUnit ∷ Eq BaseUnit where
  eq (BaseUnit u1) (BaseUnit u2) =     u1.long == u2.long
                                &&    u1.short == u2.short
                                && u1.unitType == u2.unitType

instance showBaseUnit ∷ Show BaseUnit where
  show = longName

-- | Test whether or not a given `BaseUnit` is a standard unit.
isStandardUnit ∷ BaseUnit → Boolean
isStandardUnit (BaseUnit u) =
  case u.unitType of
    Standard → true
    _        → false

-- | Convert a base unit to a standard unit.
baseToStandard ∷ BaseUnit → DerivedUnit
baseToStandard bu@(BaseUnit u) =
  case u.unitType of
    Standard → fromBaseUnit bu
    NonStandard { standardUnit, factor } → standardUnit

conversionFactor ∷ BaseUnit → ConversionFactor
conversionFactor (BaseUnit u) =
  case u.unitType of
    Standard → one
    NonStandard { standardUnit, factor } → factor

-- | A number that represents a power of ten as a prefix for a unit.
type Prefix = Decimal

-- | A number that represents the exponent of a unit, like *2* in *m²*.
type Exponent = Number

-- | Type alias for something like *m³*, *s⁻¹*, *km²* or similar A prefix.
-- | value of `p` represents an additional factor of `10^p`. The mathematical
-- | representation of this record would be *(10^prefix * baseUnit)^exponent*.
type BaseUnitWithExponent = { prefix   ∷ Prefix
                            , baseUnit ∷ BaseUnit
                            , exponent ∷ Exponent }

-- | A generic physical unit. The `Semigroup`/`Monoid` instance implements
-- | multiplication of units.
-- |
-- | Implementation detail:
-- | A `DerivedUnit` is a product of `BaseUnits`, raised to arbitrary powers.
-- | Each factor also has a `Prefix` value which represents a numerical
-- | prefix as a power of ten.
data DerivedUnit = DerivedUnit (List BaseUnitWithExponent)

-- | Expose the underlying list of base units.
runDerivedUnit ∷ DerivedUnit → List BaseUnitWithExponent
runDerivedUnit (DerivedUnit u) = u

-- | Add a given prefix value to a unit: `withPrefix 3.0 meter = kilo meter`.
withPrefix ∷ Number → DerivedUnit → DerivedUnit
withPrefix p (DerivedUnit Nil) =
  DerivedUnit $ singleton { prefix: fromNumber p, baseUnit: unity', exponent: 1.0 }
withPrefix p (DerivedUnit us) = DerivedUnit $
  case findIndex (\u → u.exponent == 1.0) us of
    Just ind →
      fromMaybe us (modifyAt ind (\u → u { prefix = u.prefix + fromNumber p }) us)
    Nothing → { prefix: fromNumber p, baseUnit: unity', exponent: 1.0 } : us

-- | Remove all prefix values from the unit:
-- | ```
-- | removePrefix (kilo meter <> milli second) = meter <> second
-- | ```
removePrefix ∷ DerivedUnit → DerivedUnit
removePrefix (DerivedUnit list) = DerivedUnit $ (_ { prefix = fromNumber 0.0 }) <$> list

-- | Like filter, but also return the non-matching elements.
split ∷ ∀ a . (a → Boolean) → List a → { yes ∷ List a, no ∷ List a }
split _ Nil = { yes: Nil, no: Nil }
split f (x : xs) =
  if f x
    then res { yes = x : res.yes }
    else res { no  = x : res.no  }
  where
    res = split f xs

-- | A variant of `Data.List.groupBy` which groups non-consecutive elements,
-- | too. Notice the worse complexity.
-- |
-- | Running time: `O(n²)`
groupBy' ∷ ∀ a. (a → a → Boolean) → List a → List (NonEmptyList a)
groupBy' _ Nil = Nil
groupBy' eq (x : xs) = case split (eq x) xs of
  { yes, no } → NonEmptyList (x :| yes) : groupBy' eq no

-- | Simplify the internal representation of a `DerivedUnit` by merging base
-- | units of the same type. For example, *m·s·m* will by simplified to *m²·s*.
simplify ∷ DerivedUnit → DerivedUnit
simplify (DerivedUnit list) = DerivedUnit (go list)
  where
    go = groupBy' (\u1 u2 → u1.baseUnit == u2.baseUnit)
           >>> map toList >>> concat
           >>> groupBy' (\u1 u2 → u1.baseUnit == u2.baseUnit
                                 && u1.prefix == u2.prefix)
           >>> map merge
           >>> filter (\x → not (x.exponent == 0.0))
    merge units = { prefix: (head units).prefix
                  , baseUnit: (head units).baseUnit
                  , exponent: sum $ _.exponent <$> units }

-- | Split up a physical units into several parts that belong to the same
-- | physical dimension (length, time, ...). In the first component, the
-- | returned tuples contain a 'target' unit, to which this group can be
-- | converted. In the second component, the original group is returned.
splitByDimension ∷ DerivedUnit → List (Tuple DerivedUnit DerivedUnit)
splitByDimension (DerivedUnit list) = transform list
  where
    transform = groupBy' (eq `on` standardUnit) >>> map reduce

    standardUnit = _.baseUnit >>> baseToStandard

    reduce ∷ NonEmptyList BaseUnitWithExponent → Tuple DerivedUnit DerivedUnit
    reduce us = Tuple first (DerivedUnit $ toList us)
      where
        first = DerivedUnit $ singleton $ (head us) { exponent = exp }
        exp = sum $ (_.exponent) <$> us

instance eqDerivedUnit ∷ Eq DerivedUnit where
  eq u1 u2 = (_.baseUnit <$> list1' == _.baseUnit <$> list2')
          && (_.exponent <$> list1' == _.exponent <$> list2')
          && globalPrefix list1 == globalPrefix list2
    where
      prepare = simplify
                >>> runDerivedUnit
                >>> sortBy (comparing (_.baseUnit >>> shortName))

      removeUnity = filter (\u → longName u.baseUnit /= "unity")

      list1 = prepare u1
      list2 = prepare u2

      list1' = removeUnity list1
      list2' = removeUnity list2

      globalPrefix ∷ List BaseUnitWithExponent → Prefix
      globalPrefix us = sum $ map (\{prefix, baseUnit, exponent} → prefix * fromNumber exponent) us

instance showDerivedUnit ∷ Show DerivedUnit where
  show (DerivedUnit us) = listString us
    where
      listString Nil       = "unity"
      listString (u : Nil) = show' u
      listString us'       = "(" <> intercalate " <> " (show' <$> us') <> ")"

      addPrf  -18.0 str = "(atto "  <> str <> ")"
      addPrf  -15.0 str = "(femto " <> str <> ")"
      addPrf  -12.0 str = "(pico "  <> str <> ")"
      addPrf   -9.0 str = "(nano "  <> str <> ")"
      addPrf   -6.0 str = "(micro " <> str <> ")"
      addPrf   -3.0 str = "(milli " <> str <> ")"
      addPrf    0.0 str = str
      addPrf    3.0 str = "(kilo "  <> str <> ")"
      addPrf    6.0 str = "(mega "  <> str <> ")"
      addPrf    9.0 str = "(giga "  <> str <> ")"
      addPrf   12.0 str = "(tera "  <> str <> ")"
      addPrf   15.0 str = "(peta "  <> str <> ")"
      addPrf   18.0 str = "(exa "   <> str <> ")"
      addPrf prefix str = "(withPrefix (" <> show prefix <> ") (" <> str <> "))"

      show' { prefix, baseUnit, exponent: 1.0 } = addPrf (toNumber prefix) (show baseUnit)
      show' { prefix, baseUnit, exponent }
        | prefix == zero =
            show baseUnit <> " .^ (" <> show exponent <> ")"
        | otherwise      =
            addPrf (toNumber prefix) (show baseUnit) <> " .^ (" <> show exponent <> ")"

instance semigroupDerivedUnit ∷ Semigroup DerivedUnit where
  append (DerivedUnit u1) (DerivedUnit u2) =
    simplify $ DerivedUnit (u1 <> u2)

instance monoidDerivedUnit ∷ Monoid DerivedUnit where
  mempty = unity

-- | Helper function to create a standard unit.
makeStandard ∷ String → String → DerivedUnit
makeStandard long short = fromBaseUnit $
  BaseUnit { short, long, unitType: Standard }

-- | Helper function to create a non-standard unit.
makeNonStandard ∷ String → String → Number → DerivedUnit
                   → DerivedUnit
makeNonStandard long short factor standardUnit = fromBaseUnit $
  BaseUnit { short, long, unitType: NonStandard { standardUnit: standardUnit
                                                , factor: fromNumber factor
                                                } }

-- | Convert all contained units to standard units and return the global
-- | conversion factor.
toStandardUnit ∷ DerivedUnit → Tuple DerivedUnit ConversionFactor
toStandardUnit (DerivedUnit units) = Tuple units' conv
  where
    conv = product (snd <$> converted)
    units' = foldMap fst converted

    converted = convert <$> units

    convert ∷ BaseUnitWithExponent → Tuple DerivedUnit Decimal
    convert { prefix, baseUnit, exponent } =
      Tuple (standardUnit .^ exponent)
            ((fromNumber 10.0 `pow` prefix * factor) `pow` exponent')
        where
          standardUnit = baseToStandard baseUnit
          factor = conversionFactor baseUnit
          exponent' = fromNumber exponent

-- | Get the name of a SI-prefix.
prefixName ∷ Prefix → Maybe String
prefixName = pn <<< toNumber
  where
    pn -18.0 = Just "a"
    pn -15.0 = Just "f"
    pn -12.0 = Just "p"
    pn  -9.0 = Just "n"
    pn  -6.0 = Just "µ"
    pn  -3.0 = Just "m"
    pn  -2.0 = Just "c"
    pn  -1.0 = Just "d"
    pn   0.0 = Just ""
    pn   2.0 = Just "h"
    pn   3.0 = Just "k"
    pn   6.0 = Just "M"
    pn   9.0 = Just "G"
    pn  12.0 = Just "T"
    pn  15.0 = Just "P"
    pn  18.0 = Just "E"
    pn     _ = Nothing

-- | Helper to show exponents in superscript notation.
prettyExponent ∷ Number → String
prettyExponent -5.0 = "⁻⁵"
prettyExponent -4.0 = "⁻⁴"
prettyExponent -3.0 = "⁻³"
prettyExponent -2.0 = "⁻²"
prettyExponent -1.0 = "⁻¹"
prettyExponent  1.0 = ""
prettyExponent  2.0 = "²"
prettyExponent  3.0 = "³"
prettyExponent  4.0 = "⁴"
prettyExponent  5.0 = "⁵"
prettyExponent exp = "^(" <> show exp <> ")"

-- | A human-readable `String` representation of a `DerivedUnit`.
toString ∷ DerivedUnit → String
toString (DerivedUnit us) = unitString
  where
    prefixName' exp = fromMaybe ("10^" <> show exp <> "·") (prefixName exp)

    withExp { prefix, baseUnit, exponent } =
      prefixName' prefix <> shortName baseUnit <> prettyExponent exponent

    usSorted = sortBy (comparing (\rec → -rec.exponent)) us
    splitted = span (\rec → rec.exponent >= 0.0) usSorted
    positiveUs = splitted.init
    negativeUs = sortBy (comparing _.exponent) splitted.rest
    reverseExp rec = rec { exponent = -rec.exponent }

    positiveUsStr = intercalate "·" (withExp <$> positiveUs)
    negativeUsStr = intercalate "·" (withExp <$> negativeUs)
    negativeUsStr' = intercalate "·" ((withExp <<< reverseExp) <$> negativeUs)

    unitString =
      case positiveUs of
        Nil → negativeUsStr
        _   → case negativeUs of
                Nil → positiveUsStr
                n : Nil → positiveUsStr <> "/" <> negativeUsStr'
                ns → positiveUsStr <> "/(" <> negativeUsStr' <> ")"

-- | Raise a unit to the given power.
power ∷ DerivedUnit → Number → DerivedUnit
power u n = DerivedUnit $ update <$> runDerivedUnit u
  where
    update rec = rec { exponent = rec.exponent * n }

infixl 9 power as .^

-- | Divide two units.
divideUnits ∷ DerivedUnit → DerivedUnit → DerivedUnit
divideUnits du1 du2 = du1 <> du2 .^ (-1.0)

infixl 6 divideUnits as ./

-- | A helper (dimensionless) unit for scalars, used internally.
unity' ∷ BaseUnit
unity' = BaseUnit { short: "unity", long: "unity", unitType: Standard }

-- | A `DerivedUnit` corresponding to `1`, i.e. the unit of scalar
-- | (or dimensionless) values.
unity ∷ DerivedUnit
unity = DerivedUnit Nil

-- | Convert a `BaseUnit` to a `DerivedUnit`.
fromBaseUnit ∷ BaseUnit → DerivedUnit
fromBaseUnit = DerivedUnit <<< singleton
                           <<< { prefix: fromNumber 0.0
                               , baseUnit: _
                               , exponent: 1.0 }

atto ∷ DerivedUnit → DerivedUnit
atto = withPrefix (-18.0)

femto ∷ DerivedUnit → DerivedUnit
femto = withPrefix (-15.0)

pico ∷ DerivedUnit → DerivedUnit
pico = withPrefix (-12.0)

nano ∷ DerivedUnit → DerivedUnit
nano = withPrefix (-9.0)

micro ∷ DerivedUnit → DerivedUnit
micro = withPrefix (-6.0)

milli ∷ DerivedUnit → DerivedUnit
milli = withPrefix (-3.0)

centi ∷ DerivedUnit → DerivedUnit
centi = withPrefix (-2.0)

deci ∷ DerivedUnit → DerivedUnit
deci = withPrefix (-1.0)

hecto ∷ DerivedUnit → DerivedUnit
hecto = withPrefix 2.0

kilo ∷ DerivedUnit → DerivedUnit
kilo = withPrefix 3.0

mega ∷ DerivedUnit → DerivedUnit
mega = withPrefix 6.0

giga ∷ DerivedUnit → DerivedUnit
giga = withPrefix 9.0

tera ∷ DerivedUnit → DerivedUnit
tera = withPrefix 12.0

peta ∷ DerivedUnit → DerivedUnit
peta = withPrefix 15.0

exa ∷ DerivedUnit → DerivedUnit
exa = withPrefix 18.0
