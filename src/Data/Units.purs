-- | This module defines data types and functions to handle physical units.
module Data.Units
  ( Prefix
  , DerivedUnit()
  , decimalPrefix
  , binaryPrefix
  , removePrefix
  , simplify
  , splitByDimension
  , baseRepresentation
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
  , quecto
  , ronto
  , yocto
  , zepto
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
  , kibi
  , mebi
  , gibi
  , tebi
  , pebi
  , exbi
  , zebi
  , yobi
  , zetta
  , yotta
  , ronna
  , quetta
  ) where

import Prelude

import Data.Decimal (Decimal, pow, fromNumber, toNumber)
import Data.Foldable (intercalate, sum, foldMap, product, fold)
import Data.Function (on)
import Data.List (List(Nil), filter, findIndex, modifyAt,
                  singleton, sortBy, span, (:), concat, uncons)
import Data.List.NonEmpty (NonEmptyList(..), head, toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un)
import Data.NonEmpty ((:|))
import Data.Pair (Pair(..))
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

derive instance Eq UnitType

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

derive newtype instance Eq BaseUnit

instance Show BaseUnit where
  show = longName

-- | Test whether or not a given `BaseUnit` is a standard unit.
isStandardUnit ∷ BaseUnit → Boolean
isStandardUnit (BaseUnit u) = u.unitType == Standard

-- | Convert a base unit to a standard unit.
baseToStandard ∷ BaseUnit → DerivedUnit
baseToStandard bu@(BaseUnit u) =
  case u.unitType of
    Standard → fromBaseUnit bu
    NonStandard { standardUnit } → standardUnit

conversionFactor ∷ BaseUnit → ConversionFactor
conversionFactor (BaseUnit u) =
  case u.unitType of
    Standard → one
    NonStandard { factor } → factor

-- | Units can have both decimal prefixes (micro, kilo, ..) or binary
-- | prefixes (kibi, mebi, ..).
data PrefixBase = Decimal | Binary

derive instance Eq PrefixBase

-- | A number that represents a power of ten/two as a prefix for a unit.
data Prefix = Prefix PrefixBase Decimal

instance Eq Prefix where
  eq (Prefix b1 e1) (Prefix b2 e2) =
    (e1 == zero && e2 == zero) || (b1 == b2 && e1 == e2)

-- | Represents a non-existing prefix
noPrefix ∷ Prefix
noPrefix = Prefix Decimal zero

-- | A number that represents the exponent of a unit, like *2* in *m²*.
type Exponent = Number

-- | Type alias for something like *m³*, *s⁻¹*, *km²* or similar. A prefix
-- | value represents an additional factor of `10^p` or `2^p`. The mathematical
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
-- | prefix as a power of ten or two.
newtype DerivedUnit = DerivedUnit (List BaseUnitWithExponent)

-- | Expose the underlying list of base units.
runDerivedUnit ∷ DerivedUnit → List BaseUnitWithExponent
runDerivedUnit (DerivedUnit u) = u

-- | Add a given prefix value to a unit: `withPrefix Decimal 3.0 meter = kilo meter`.
withPrefix ∷ PrefixBase → Number → DerivedUnit → DerivedUnit
withPrefix base p (DerivedUnit Nil) =
  DerivedUnit $ singleton { prefix: Prefix base (fromNumber p), baseUnit: unity', exponent: 1.0 }
withPrefix base p (DerivedUnit us) = DerivedUnit
  case findIndex isPlaceholder us of
    Just ind →
      fromMaybe us (modifyAt ind addPrefixExp us)
    Nothing → { prefix: Prefix base (fromNumber p), baseUnit: unity', exponent: 1.0 } : us
  where
    isPlaceholder { prefix: Prefix b prf, exponent } =
      exponent == 1.0 && (base == b || prf == zero)
    addPrefixExp du@{ prefix: Prefix _ dp } =
      du { prefix = Prefix base (fromNumber p + dp) }

-- | Add a given decimal prefix value to a unit: `withDecimal 3.0 meter = kilo meter`.
decimalPrefix ∷ Number → DerivedUnit → DerivedUnit
decimalPrefix = withPrefix Decimal

-- | Add a given binary prefix value to a unit: `withDecimal 10.0 byte = kibi byte`.
binaryPrefix ∷ Number → DerivedUnit → DerivedUnit
binaryPrefix = withPrefix Binary

-- | Remove all prefix values from the unit:
-- | ```
-- | removePrefix (kilo meter <> milli second) = meter <> second
-- | ```
removePrefix ∷ DerivedUnit → DerivedUnit
removePrefix (DerivedUnit list) = DerivedUnit $ (_ { prefix = noPrefix }) <$> list

-- | Like filter, but also return the non-matching elements.
split ∷ ∀ a. (a → Boolean) → List a → { yes ∷ List a, no ∷ List a }
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

-- | A version of `sortBy` for `NonEmptyList`.
sortBy' ∷ ∀ a. (a → a → Ordering) → NonEmptyList a → NonEmptyList a
sortBy' f (NonEmptyList (x :| xs)) = NonEmptyList nel
  where
    sorted = sortBy f (x : xs)
    nel = case uncons sorted of
            Just { head, tail } → head :| tail
            Nothing             → x    :| xs   -- This can never happen

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
           >>> filter (\x → x.exponent /= 0.0)
    merge units = (head units) { exponent = sum $ _.exponent <$> units }

-- | Split up a physical unit into several parts that belong to the same
-- | physical dimension (length, time, ...). In the first component, the
-- | returned tuples contain a 'target' unit, to which this group can be
-- | converted. In the second component, the original group is returned.
splitByDimension ∷ DerivedUnit → List (Tuple DerivedUnit DerivedUnit)
splitByDimension (DerivedUnit list) = transform list
  where
    transform = groupBy' (eq `on` standardUnitWithoutExponent) >>> map reduce

    -- Remove the exponent of a derived unit if *and only if* it exists of
    -- a single component.
    removeExponent ∷ DerivedUnit → DerivedUnit
    removeExponent (DerivedUnit (u : Nil)) = DerivedUnit $ singleton (u { exponent = 1.0 })
    removeExponent us = us

    -- Get the standard unit of a given base unit, e.g. Hz -> s^(-1)
    standardUnit ∷ BaseUnitWithExponent → DerivedUnit
    standardUnit = _.baseUnit >>> baseToStandard

    -- Get the standard unit without exponent, e.g. Hz -> s
    standardUnitWithoutExponent = standardUnit >>> removeExponent

    -- Get the unit exponent with respect to the given base.
    exponentWRT ∷ BaseUnitWithExponent → BaseUnitWithExponent → Number
    exponentWRT base u = u.exponent * removedExponent u / removedExponent base
      where
        removedExponent u' =
          case standardUnit u' of
            DerivedUnit (su : Nil) → su.exponent
            _ → 1.0

    -- | Favor standard units over non-standard units, favor lower exponents
    -- | over larger exponents.
    -- | This way, `ft·m` will be converted to `m²` and `liter/m` will be
    -- | converted to `m²`.
    heuristic ∷ BaseUnitWithExponent → BaseUnitWithExponent → Ordering
    heuristic b1 b2 =
         compare (isStandardUnit b1.baseUnit) (isStandardUnit b2.baseUnit)
      <> compare b2.exponent b1.exponent

    reduce ∷ NonEmptyList BaseUnitWithExponent → Tuple DerivedUnit DerivedUnit
    reduce us' = Tuple convertTo (DerivedUnit $ toList us)
      where
        us = sortBy' (flip heuristic) us'
        first = head us
        convertTo = DerivedUnit $ singleton $ first { exponent = exp }
        exp = sum $ exponentWRT first <$> us

-- | Return a representation of the `DerivedUnit` in terms of base units, split
-- | by physical dimension.
baseRepresentation ∷ DerivedUnit → List DerivedUnit
baseRepresentation du
  | du == unity = singleton du
  | otherwise = us
  where
    du' = fst $ toStandardUnit du
    us = (replace <<< snd) <$> splitByDimension du'

    -- Replace "gram" by "kilo gram"
    replace u =
      case u of
        DerivedUnit (b@{ baseUnit: BaseUnit rec@{ long: "gram" } } : Nil) →
          DerivedUnit (b { baseUnit = BaseUnit (rec { long = "kilogram", short = "kg" }) } : Nil)
        _ → u

instance Eq DerivedUnit where
  eq u1 u2 = (_.baseUnit <$> list1') == (_.baseUnit <$> list2')
          && (_.exponent <$> list1') == (_.exponent <$> list2')
          && globalPrefix list1 == globalPrefix list2
    where
      prepare = simplify
                >>> runDerivedUnit
                >>> sortBy (comparing (_.baseUnit >>> shortName))

      removeUnity = filter \u → longName u.baseUnit /= "unity"

      list1 = prepare u1
      list2 = prepare u2

      list1' = removeUnity list1
      list2' = removeUnity list2

      globalPrefix ∷ List BaseUnitWithExponent → Pair Decimal
      globalPrefix us = map (un Additive) $ fold (map (map Additive <<< prefixPair) us)
        where
          prefixPair {prefix, exponent} = (_ * fromNumber exponent) <$> toPair prefix

          toPair (Prefix Decimal p) = Pair p zero
          toPair (Prefix Binary p)  = Pair zero p

instance Show DerivedUnit where
  show (DerivedUnit us) = listString us
    where
      listString Nil       = "unity"
      listString (u : Nil) = show' u
      listString us'       = "(" <> intercalate " <> " (show' <$> us') <> ")"

      addPrf Decimal = decPrf
      addPrf Binary = binPrf

      decPrf  (-30.0) str = "(quecto "  <> str <> ")"
      decPrf  (-27.0) str = "(ronto "  <> str <> ")"
      decPrf  (-24.0) str = "(yocto "  <> str <> ")"
      decPrf  (-21.0) str = "(zepto "  <> str <> ")"
      decPrf  (-18.0) str = "(atto "  <> str <> ")"
      decPrf  (-15.0) str = "(femto " <> str <> ")"
      decPrf  (-12.0) str = "(pico "  <> str <> ")"
      decPrf  ( -9.0) str = "(nano "  <> str <> ")"
      decPrf  ( -6.0) str = "(micro " <> str <> ")"
      decPrf  ( -3.0) str = "(milli " <> str <> ")"
      decPrf     0.0  str = str
      decPrf     3.0  str = "(kilo "  <> str <> ")"
      decPrf     6.0  str = "(mega "  <> str <> ")"
      decPrf     9.0  str = "(giga "  <> str <> ")"
      decPrf    12.0  str = "(tera "  <> str <> ")"
      decPrf    15.0  str = "(peta "  <> str <> ")"
      decPrf    18.0  str = "(exa "   <> str <> ")"
      decPrf    21.0  str = "(zetta "   <> str <> ")"
      decPrf    24.0  str = "(yotta "   <> str <> ")"
      decPrf    27.0  str = "(ronna "   <> str <> ")"
      decPrf    30.0  str = "(quetta "   <> str <> ")"
      decPrf  prefix  str = "(decimalPrefix (" <> show prefix <> ") (" <> str <> "))"

      binPrf  prefix  str = "(binaryPrefix (" <> show prefix <> ") (" <> str <> "))"

      show' { prefix: Prefix base p, baseUnit, exponent }
        | exponent == 1.0 = addPrf base (toNumber p) (show baseUnit)
        | p == zero =
            show baseUnit <> " .^ (" <> show exponent <> ")"
        | otherwise =
            addPrf base (toNumber p) (show baseUnit) <> " .^ (" <> show exponent <> ")"

instance Semigroup DerivedUnit where
  append (DerivedUnit u1) (DerivedUnit u2) =
    simplify $ DerivedUnit (u1 <> u2)

instance Monoid DerivedUnit where
  mempty = unity

-- | Helper function to create a standard unit.
makeStandard ∷ String → String → DerivedUnit
makeStandard long short = fromBaseUnit $
  BaseUnit { short, long, unitType: Standard }

-- | Helper function to create a non-standard unit.
makeNonStandard ∷ String → String → Number → DerivedUnit
                   → DerivedUnit
makeNonStandard long short factor standardUnit = fromBaseUnit $
  BaseUnit { short, long, unitType: NonStandard { standardUnit
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
    convert { prefix: Prefix base p, baseUnit, exponent } =
      Tuple (standardUnit .^ exponent)
            ((toNum base `pow` p * factor) `pow` exponent')
        where
          toNum Decimal = fromNumber 10.0
          toNum Binary = fromNumber 2.0
          standardUnit = baseToStandard baseUnit
          factor = conversionFactor baseUnit
          exponent' = fromNumber exponent

-- | Get the name of a SI-prefix.
prefixName ∷ Prefix → Maybe String
prefixName (Prefix Decimal p) = pn (toNumber p)
  where
    pn (-30.0) = Just "q"
    pn (-27.0) = Just "r"
    pn (-24.0) = Just "y"
    pn (-21.0) = Just "z"
    pn (-18.0) = Just "a"
    pn (-12.0) = Just "p"
    pn (-15.0) = Just "f"
    pn ( -9.0) = Just "n"
    pn ( -6.0) = Just "µ"
    pn ( -3.0) = Just "m"
    pn ( -2.0) = Just "c"
    pn ( -1.0) = Just "d"
    pn    0.0  = Just ""
    pn    2.0  = Just "h"
    pn    3.0  = Just "k"
    pn    6.0  = Just "M"
    pn    9.0  = Just "G"
    pn   12.0  = Just "T"
    pn   15.0  = Just "P"
    pn   18.0  = Just "E"
    pn   21.0  = Just "Z"
    pn   24.0  = Just "Y"
    pn   27.0  = Just "R"
    pn   30.0  = Just "Q"
    pn      _  = Nothing
prefixName (Prefix Binary p) = pn (toNumber p)
  where
    pn  0.0 = Just ""
    pn 10.0 = Just "Ki"
    pn 20.0 = Just "Mi"
    pn 30.0 = Just "Gi"
    pn 40.0 = Just "Ti"
    pn 50.0 = Just "Pi"
    pn 60.0 = Just "Ei"
    pn 70.0 = Just "Zi"
    pn 80.0 = Just "Yi"
    pn    _ = Nothing

-- | Helper to show exponents in superscript notation.
prettyExponent ∷ Number → String
prettyExponent (-5.0) = "⁻⁵"
prettyExponent (-4.0) = "⁻⁴"
prettyExponent (-3.0) = "⁻³"
prettyExponent (-2.0) = "⁻²"
prettyExponent (-1.0) = "⁻¹"
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
    toNum Decimal = 10
    toNum Binary = 2
    prefixName' prefix@(Prefix base exp) =
      fromMaybe (show (toNum base) <> "^" <> show exp <> "·") (prefixName prefix)

    withExp { prefix, baseUnit, exponent } =
      prefixName' prefix <> shortName baseUnit <> prettyExponent exponent

    usSorted = sortBy (comparing \rec → -rec.exponent) us
    splitted = span (\rec → rec.exponent >= 0.0) usSorted
    positiveUs = splitted.init
    negativeUs = sortBy (comparing _.exponent) splitted.rest
    reverseExp rec = rec { exponent = -rec.exponent }

    positiveUsStr = intercalate "·" (withExp <$> positiveUs)
    negativeUsStr = intercalate "·" (withExp <$> negativeUs)
    negativeUsStr' = intercalate "·" ((withExp <<< reverseExp) <$> negativeUs)

    unitString =
      case positiveUs, negativeUs of
        Nil, _     → negativeUsStr
        _, Nil     → positiveUsStr
        _, _ : Nil → positiveUsStr <> "/" <> negativeUsStr'
        _, _       → positiveUsStr <> "/(" <> negativeUsStr' <> ")"

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
                           <<< { prefix: noPrefix
                               , baseUnit: _
                               , exponent: 1.0 }

quecto ∷ DerivedUnit → DerivedUnit
quecto = decimalPrefix (-30.0)

ronto ∷ DerivedUnit → DerivedUnit
ronto = decimalPrefix (-27.0)

yocto ∷ DerivedUnit → DerivedUnit
yocto = decimalPrefix (-24.0)

zepto ∷ DerivedUnit → DerivedUnit
zepto = decimalPrefix (-21.0)

atto ∷ DerivedUnit → DerivedUnit
atto = decimalPrefix (-18.0)

femto ∷ DerivedUnit → DerivedUnit
femto = decimalPrefix (-15.0)

pico ∷ DerivedUnit → DerivedUnit
pico = decimalPrefix (-12.0)

nano ∷ DerivedUnit → DerivedUnit
nano = decimalPrefix (-9.0)

micro ∷ DerivedUnit → DerivedUnit
micro = decimalPrefix (-6.0)

milli ∷ DerivedUnit → DerivedUnit
milli = decimalPrefix (-3.0)

centi ∷ DerivedUnit → DerivedUnit
centi = decimalPrefix (-2.0)

deci ∷ DerivedUnit → DerivedUnit
deci = decimalPrefix (-1.0)

hecto ∷ DerivedUnit → DerivedUnit
hecto = decimalPrefix 2.0

kilo ∷ DerivedUnit → DerivedUnit
kilo = decimalPrefix 3.0

mega ∷ DerivedUnit → DerivedUnit
mega = decimalPrefix 6.0

giga ∷ DerivedUnit → DerivedUnit
giga = decimalPrefix 9.0

tera ∷ DerivedUnit → DerivedUnit
tera = decimalPrefix 12.0

peta ∷ DerivedUnit → DerivedUnit
peta = decimalPrefix 15.0

exa ∷ DerivedUnit → DerivedUnit
exa = decimalPrefix 18.0

zetta ∷ DerivedUnit → DerivedUnit
zetta = decimalPrefix 21.0

yotta ∷ DerivedUnit → DerivedUnit
yotta = decimalPrefix 24.0

ronna ∷ DerivedUnit → DerivedUnit
ronna = decimalPrefix 27.0

quetta ∷ DerivedUnit → DerivedUnit
quetta = decimalPrefix 30.0

kibi ∷ DerivedUnit → DerivedUnit
kibi = binaryPrefix 10.0

mebi ∷ DerivedUnit → DerivedUnit
mebi = binaryPrefix 20.0

gibi ∷ DerivedUnit → DerivedUnit
gibi = binaryPrefix 30.0

tebi ∷ DerivedUnit → DerivedUnit
tebi = binaryPrefix 40.0

pebi ∷ DerivedUnit → DerivedUnit
pebi = binaryPrefix 50.0

exbi ∷ DerivedUnit → DerivedUnit
exbi = binaryPrefix 60.0

zebi ∷ DerivedUnit → DerivedUnit
zebi = binaryPrefix 70.0

yobi ∷ DerivedUnit → DerivedUnit
yobi = binaryPrefix 80.0
