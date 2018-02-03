-- | Some currencies. Conversions between currencies are not supported.
module Data.Units.Currency where

import Data.Units (DerivedUnit, makeStandard)

-- | The United States dollar.
dollar ∷ DerivedUnit
dollar = makeStandard "dollar" "USD"

-- | The official currency of the European Union
euro ∷ DerivedUnit
euro = makeStandard "euro" "EUR"
