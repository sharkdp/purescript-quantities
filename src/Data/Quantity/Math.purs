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

pi ∷ Quantity
pi = scalar' Decimal.pi

e ∷ Quantity
e = scalar' Decimal.e
