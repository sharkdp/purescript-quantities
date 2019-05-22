-- | A collection of parts-per-x units.
module Data.Units.PartsPerX where

import Data.Units (DerivedUnit, makeNonStandard, unity)

-- | 'Dimensionless' ratio *1 pct = 1e-2*.
percent ∷ DerivedUnit
percent = makeNonStandard "percent" "pct" 1e-2 unity

-- | 'Dimensionless' ratio *1 ppm = 1e-6*.
partsPerMillion ∷ DerivedUnit
partsPerMillion = makeNonStandard "parts-per-million" "ppm" 1e-6 unity

-- | 'Dimensionless' ratio *1 ppb = 1e-9*.
partsPerBillion ∷ DerivedUnit
partsPerBillion = makeNonStandard "parts-per-billion" "ppb" 1e-9 unity

-- | 'Dimensionless' ratio *1 ppt = 1e-12*.
partsPerTrillion ∷ DerivedUnit
partsPerTrillion = makeNonStandard "parts-per-trillion" "ppt" 1e-12 unity

-- | 'Dimensionless' ratio *1 ppq = 1e-15*.
partsPerQuadrillion ∷ DerivedUnit
partsPerQuadrillion = makeNonStandard "parts-per-quadrillion" "ppq" 1e-15 unity
