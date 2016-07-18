module Data.Quantity
  ( Quantity(..)
  , (.*)
  , approximatelyEqual
  , dUnit
  , toSI
  ) where

import Prelude

import Data.Tuple (Tuple(..))

import Data.DerivedUnit (DerivedUnit, toString)
import Data.DerivedUnit as D

import Math (abs)

-- | A physical quantity.
data Quantity = Quantity Number DerivedUnit

infix 3 Quantity as .*

instance eqQuantity :: Eq Quantity where
  eq q1 q2 = value q1' == value q2' && dUnit q1' == dUnit q2'
    where
      q1' = toSI q1
      q2' = toSI q2


instance showQuantity :: Show Quantity where
  show (num .* unit) = show num <> toString unit

approximatelyEqual :: Number → Quantity → Quantity → Boolean
approximatelyEqual tol q1' q2' =
  dUnit q1 == dUnit q2 &&
  abs (v1 - v2) < tol * abs (v1 + v2)
    where
      q1 = toSI q1'
      q2 = toSI q2'
      v1 = value q1
      v2 = value q2

-- | The numerical value stored inside a `Quantity`. For internal use only
-- | (bare `Number`s without units should be handled with care).
value :: Quantity → Number
value (v .* _) = v

dUnit :: Quantity → DerivedUnit
dUnit (_ .* u) = u

data UnificationError = UnificationError DerivedUnit DerivedUnit

instance showUnificationError :: Show UnificationError where
  show (UnificationError u1 u2) = "Cannot unify unit '" <> toString u1 <>
                                  "' with unit '" <> toString u2 <> "'"


toSI :: Quantity → Quantity
toSI (num .* du) =
  case D.toSI du of
    Tuple du' conversion -> (conversion * num) .* du'

{-- convert :: Quantity → DerivedUnit → Either UnificationError Quantity --}
{-- convert q@(v .* u) u' --}
{--   | u == u' = Right q --}
{--   | otherwise = --}
{--       let q' = toSI q --}

{--       in --}
{--         if qUnit q' == u' --}
{--           then Right q' --}
{--           else Left $ UnificationError u u' --}

{-- qAdd :: Quantity → Quantity → Either UnificationError Quantity --}
{-- qAdd (v1 ⋅ u1) q2 = do --}
{--   q2' <- convert q2 u1 --}
{--   case q2' of --}
{--       (v2 ⋅ _) → pure $ (v1 + v2) ⋅ u1 --}

{-- infixl 6 qAdd as .+. --}

{-- infixl 7 mul as * --}
