module Data.Units.SI where

import Data.Units (DerivedUnit, makeStandard)

meter :: DerivedUnit
meter = makeStandard "meter" "m"

meters :: DerivedUnit
meters = meter

second :: DerivedUnit
second = makeStandard "second" "s"

seconds :: DerivedUnit
seconds = second

gram :: DerivedUnit
gram = makeStandard "gram" "g"

grams :: DerivedUnit
grams = gram
