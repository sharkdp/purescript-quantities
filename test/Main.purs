module Test.Main where

import Prelude

import Data.Tuple (fst)

import Data.DerivedUnit (meter, second, minute, hour, inch, unity, toString, (.^),
                         (./))
import Data.DerivedUnit as D
import Data.Quantity (Quantity, (.*), (⊗))
import Data.Quantity as Q

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (equal)

-- | Test if two quantities are almost equal, i.e. if the units match and the
-- | numerical value is approximately the same.
almostEqual :: ∀ e. Quantity → Quantity → Test e
almostEqual expected actual = do
  if expected `approximatelyEqual` actual
    then success
    else failure $ "expected " <> show expected <>
                   ", got " <> show actual
  where approximatelyEqual = Q.approximatelyEqual 0.00001


main :: Eff (console :: CONSOLE, testOutput :: TESTOUTPUT) Unit
main = runTest do

  suite "DerivedUnit" do
    test "Commutative Monoid" do
      equal meter (meter <> unity)
      equal meter (unity <> meter)
      equal (meter <> meter <> second) (meter <> second <> meter)
      equal (meter <> meter <> second) (second <> meter <> meter)

    test "toString" do
      equal "m" $ toString meter

    test "Show instance" do
      equal "meter" $ show meter
      equal "meter .^ 2.0" $ show (meter .^ 2.0)
      equal "meter .^ 2.0 <> second" $ show (meter .^ 2.0 <> second)

    test "toSI" do
      equal second $ fst (D.toSI minute)

  suite "Quantity" do
    test "Show instance" do
      equal "3.0m" $ show (3.0 .* meter)
      equal "3.0m²·s" $ show (3.0 .* (meter <> second <> meter))

    test "Unit conversion" do
      almostEqual (120.0 .* second) (2.0 .* minute)
      almostEqual (1.0 .* meter .^ 2.0) (1550.0031 .* inch .^ 2.0)
      almostEqual (50.0 .* meter ./ second) (180000.0 .* meter ./ hour)

    test "Multiplication" do
      equal (15.0 .* meter .^ 2.0) (3.0 .* meter ⊗ 5.0 .* meter)
