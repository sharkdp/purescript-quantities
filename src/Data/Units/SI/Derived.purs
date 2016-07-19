module Data.Units.SI.Derived where

import Data.Units (DerivedUnit, (./), (.^), unity)
import Data.Units.SI

import Prelude ((<>))

radian :: DerivedUnit
radian = unity

herz :: DerivedUnit
herz = unity ./ second
