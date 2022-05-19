-- | This module defines mathematical operations on physical quantities. Most
-- | of these functions return `Either ConversionError Quantity` since they
-- | can only be applied to scalar (dimensionless) values. If the conversion
-- | to a scalar fails, they will return a `ConversionError`.
module Data.Quantity.Math
  ( acos
  , asin
  , atan
  , atan2
  , cos
  , exp
  , ln
  , sin
  , tan
  , sinh
  , cosh
  , tanh
  , asinh
  , acosh
  , atanh
  , ceil
  , floor
  , log10
  , max2
  , max
  , min2
  , min
  , mean
  , modulo
  , round
  , gamma
  , factorial
  , pi
  , e
  , tau
  , phi
  ) where

import Prelude
import Data.Decimal as Decimal
import Data.List.NonEmpty (NonEmptyList, head, tail, length)
import Data.Foldable (foldM)
import Data.Decimal (Decimal)
import Data.Either (Either)
import Data.Quantity (Quantity, ConversionError, derivedUnit, asValueIn',
                      scalar', quantity', toScalar', (⊘), (⊕), (.*))

type Result = Either ConversionError Quantity

lift ∷ (Decimal → Decimal) → Quantity → Result
lift fn q = (scalar' <<< fn) <$> toScalar' q

lift2 ∷ (Decimal → Decimal → Decimal) → Quantity → Quantity → Result
lift2 f q1 q2 = do
  let u = derivedUnit q1
  v1 ← q1 `asValueIn'` u
  v2 ← q2 `asValueIn'` u
  pure $ quantity' (f v1 v2) u

acos ∷ Quantity → Result
acos = lift Decimal.acos

asin ∷ Quantity → Result
asin = lift Decimal.asin

atan ∷ Quantity → Result
atan = lift Decimal.atan

atan2 ∷ Quantity → Quantity → Result
atan2 x y = removeDims <$> lift2 Decimal.atan2 x y
  where
    removeDims q = q ⊘ 1.0 .* derivedUnit q

cos ∷ Quantity → Result
cos = lift Decimal.cos

exp ∷ Quantity → Result
exp = lift Decimal.exp

ln ∷ Quantity → Result
ln = lift Decimal.ln

sin ∷ Quantity → Result
sin = lift Decimal.sin

tan ∷ Quantity → Result
tan = lift Decimal.tan

sinh ∷ Quantity → Result
sinh = lift Decimal.sinh

cosh ∷ Quantity → Result
cosh = lift Decimal.cosh

tanh ∷ Quantity → Result
tanh = lift Decimal.tanh

asinh ∷ Quantity → Result
asinh = lift Decimal.asinh

acosh ∷ Quantity → Result
acosh = lift Decimal.acosh

atanh ∷ Quantity → Result
atanh = lift Decimal.atanh

ceil ∷ Quantity → Result
ceil = lift Decimal.ceil

floor ∷ Quantity → Result
floor = lift Decimal.floor

log10 ∷ Quantity → Result
log10 = lift Decimal.log10

max2 ∷ Quantity → Quantity → Result
max2 = lift2 Decimal.max

max ∷ NonEmptyList Quantity → Result
max xs = foldM max2 (head xs) (tail xs)

min2 ∷ Quantity → Quantity → Result
min2 = lift2 Decimal.min

min ∷ NonEmptyList Quantity → Result
min xs = foldM min2 (head xs) (tail xs)

mean ∷ NonEmptyList Quantity → Result
mean xs = (_ ⊘ n) <$> foldM (⊕) (head xs) (tail xs)
  where
    n = scalar' (Decimal.fromInt (length xs))

modulo ∷ Quantity → Quantity → Result
modulo = lift2 Decimal.modulo

round ∷ Quantity → Result
round = lift Decimal.round

gamma ∷ Quantity → Result
gamma = lift Decimal.gamma

factorial ∷ Quantity → Result
factorial = lift Decimal.factorial

pi ∷ Quantity
pi = scalar' Decimal.pi

e ∷ Quantity
e = scalar' Decimal.e

tau ∷ Quantity
tau = scalar' $ Decimal.fromNumber 2.0 * Decimal.pi

phi ∷ Quantity
phi = scalar' $ (one + Decimal.sqrt (Decimal.fromNumber 5.0)) / Decimal.fromNumber 2.0
