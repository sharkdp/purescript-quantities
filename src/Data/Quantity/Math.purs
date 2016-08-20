module Data.Quantity.Math where

import Prelude
import Math as M
import Data.Either (Either)
import Data.Quantity (Quantity, UnificationError, asValueIn, scalar)
import Data.Units (unity)

type Result = Either UnificationError Quantity

lift :: (Number → Number) → Quantity → Result
lift fn q = (scalar <<< fn) <$> (q `asValueIn` unity)

lift2 :: (Number → Number → Number) → Quantity → Quantity → Result
lift2 fn q1 q2 = scalar <$> (fn <$> (q1 `asValueIn` unity)
                                <*> (q2 `asValueIn` unity))

acos :: Quantity → Result
acos = lift M.acos

asin :: Quantity → Result
asin = lift M.asin

atan :: Quantity → Result
atan = lift M.atan

atan2 :: Quantity → Quantity → Result
atan2 = lift2 M.atan2

cos :: Quantity → Result
cos = lift M.cos

exp :: Quantity → Result
exp = lift M.exp

log :: Quantity → Result
log = lift M.log

sin :: Quantity → Result
sin = lift M.sin

tan :: Quantity → Result
tan = lift M.tan
