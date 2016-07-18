module Data.BaseUnit
  ( ConversionFactor
  , BaseUnit()
  , shortName
  , longName
  , isStandardUnit
  , toStandardUnit
  , conversionFactor
  -- Standard (SI) units
  , meter
  , gram
  , second
  -- Non-standard units
  , minute
  , hour
  , inch
  ) where

import Prelude

type ConversionFactor = Number

-- | A base unit can either be a standardized SI unit or some non-standard
-- | unit. In the latter case, a conversion to a SI unit must be provided.
data UnitType
  = Standard
  | NonStandard
      { standardUnit :: BaseUnit
      , factor       :: ConversionFactor
      }

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
  -- TODO: this is horrible!
  eq (BaseUnit u1) (BaseUnit u2) = u1.long == u2.long

instance showBaseUnit :: Show BaseUnit where
  show = longName

-- | Test whether or not a given `BaseUnit` is a standard (SI) unit.
isStandardUnit :: BaseUnit → Boolean
isStandardUnit (BaseUnit u) =
  case u.unitType of
    Standard → true
    _        → false

-- | Convert a unit to a standard (SI) unit.
toStandardUnit :: BaseUnit → BaseUnit
toStandardUnit bu@(BaseUnit u) =
  case u.unitType of
      Standard → bu
      NonStandard { standardUnit, factor } → standardUnit

conversionFactor :: BaseUnit → ConversionFactor
conversionFactor (BaseUnit u) =
  case u.unitType of
      Standard → 1.0
      NonStandard { standardUnit, factor } → factor

-- | Helper function to create a standard (SI) unit.
makeStandard :: String → String → BaseUnit
makeStandard long short = BaseUnit { short, long, unitType: Standard }

-- | Helper function to create a non-SI unit.
makeNonStandard :: String → String → BaseUnit → ConversionFactor → BaseUnit
makeNonStandard long short standardUnit factor =
  BaseUnit { short, long, unitType: NonStandard { standardUnit, factor } }

meter :: BaseUnit
meter = makeStandard "meter" "m"

second :: BaseUnit
second = makeStandard "second" "s"

gram :: BaseUnit
gram = makeStandard "gram" "g"

inch :: BaseUnit
inch = makeNonStandard "inch" "in" meter 0.0254

minute :: BaseUnit
minute = makeNonStandard "minute" "min" second 60.0

hour :: BaseUnit
hour = makeNonStandard "hour" "h" second 3600.0
