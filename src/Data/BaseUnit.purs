module Data.BaseUnit
  ( ConversionFactor
  , BaseUnit()
  , shortName
  , longName
  , isSI
  , toSI
  , conversionFactor
  -- SI units
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
  = SI
  | NonStandard
      { siUnit     :: BaseUnit
      , conversion :: ConversionFactor
      }

-- | The type for a base unit.
newtype BaseUnit = BaseUnit
  { long :: String
  , short :: String
  , unitType :: UnitType
  }

-- | The short name of a base unit (m, s, ..).
shortName :: BaseUnit → String
shortName (BaseUnit u) = u.short

-- | The long name of a base unit (meter, second, ..).
longName :: BaseUnit → String
longName (BaseUnit u) = u.long

instance eqBaseUnit :: Eq BaseUnit where
  eq (BaseUnit u1) (BaseUnit u2) = u1.long == u2.long -- TODO

instance showBaseUnit :: Show BaseUnit where
  show = longName

makeSI :: String → String → BaseUnit
makeSI long short = BaseUnit { short, long, unitType: SI }

nonStandardUnit :: String → String → BaseUnit → ConversionFactor → BaseUnit
nonStandardUnit short long siUnit conversion =
  BaseUnit { short, long, unitType: NonStandard { siUnit, conversion } }

meter :: BaseUnit
meter = makeSI "meter" "m"

second :: BaseUnit
second = makeSI "second" "s"

gram :: BaseUnit
gram = makeSI "gram" "g"

inch :: BaseUnit
inch = nonStandardUnit "inch" "in" meter 0.0254

minute :: BaseUnit
minute = nonStandardUnit "minute" "min" second 60.0

hour :: BaseUnit
hour = nonStandardUnit "hour" "h" second 3600.0

isSI :: BaseUnit → Boolean
isSI (BaseUnit u) =
  case u.unitType of
    SI → true
    _  → false

toSI :: BaseUnit → BaseUnit
toSI (BaseUnit u) =
  case u.unitType of
      SI → BaseUnit u
      NonStandard { siUnit, conversion } → siUnit

conversionFactor :: BaseUnit → ConversionFactor
conversionFactor (BaseUnit u) =
  case u.unitType of
      SI → 1.0
      NonStandard { siUnit, conversion } → conversion
