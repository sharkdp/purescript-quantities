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
  , max
  , min
  , round
  , pi
  , e
  ) where

import Prelude
import Data.Either (Either)
import Data.Quantity (Quantity, UnificationError, asValueIn', scalar')
import Data.Units (unity)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal

type Result = Either UnificationError Quantity

lift :: (Decimal → Decimal) → Quantity → Result
lift fn q = (scalar' <<< fn) <$> (q `asValueIn'` unity)

lift2 :: (Decimal → Decimal → Decimal) → Quantity → Quantity → Result
lift2 fn q1 q2 = scalar' <$> (fn <$> (q1 `asValueIn'` unity)
                                 <*> (q2 `asValueIn'` unity))

acos :: Quantity → Result
acos = lift Decimal.acos

asin :: Quantity → Result
asin = lift Decimal.asin

atan :: Quantity → Result
atan = lift Decimal.atan

atan2 :: Quantity → Quantity → Result
atan2 = lift2 Decimal.atan2

cos :: Quantity → Result
cos = lift Decimal.cos

exp :: Quantity → Result
exp = lift Decimal.exp

ln :: Quantity → Result
ln = lift Decimal.ln

sin :: Quantity → Result
sin = lift Decimal.sin

tan :: Quantity → Result
tan = lift Decimal.tan

sinh :: Quantity → Result
sinh = lift Decimal.sinh

cosh :: Quantity → Result
cosh = lift Decimal.cosh

tanh :: Quantity → Result
tanh = lift Decimal.tanh

asinh :: Quantity → Result
asinh = lift Decimal.asinh

acosh :: Quantity → Result
acosh = lift Decimal.acosh

atanh :: Quantity → Result
atanh = lift Decimal.atanh

ceil :: Quantity → Result
ceil = lift Decimal.ceil

floor :: Quantity → Result
floor = lift Decimal.floor

log10 :: Quantity → Result
log10 = lift Decimal.log10

max :: Quantity → Quantity → Result
max = lift2 Decimal.max

min :: Quantity → Quantity → Result
min = lift2 Decimal.min

round :: Quantity → Result
round = lift Decimal.round

pi ∷ Quantity
pi = scalar' Decimal.pi

e ∷ Quantity
e = scalar' Decimal.e
