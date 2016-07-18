module Test.Main where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Tuple (fst)

import Data.DerivedUnit (meter, meters, second, seconds, minute, minutes, hour,
                         hours, inch, inches, unity, toString, (.^), (./))
import Data.DerivedUnit as D
import Data.Quantity (Quantity, (.*), (⊕), (⊗), valueIn)
import Data.Quantity as Q

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, equal)

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
      almostEqual (120.0 .* seconds) (2.0 .* minutes)
      almostEqual (1.0 .* meter .^ 2.0) (1550.0031 .* inch .^ 2.0)
      almostEqual (50.0 .* meters ./ second) (180000.0 .* meters ./ hour)

    test "Conversion" do
      equal (Right $ 2.54) (valueIn meters (100.0 .* inches))
      equal (Right $ 100.0) (valueIn inches (2.54 .* meters))
      equal (Right $ 120.0) (valueIn minutes (2.0 .* hours))
      equal (Right $ 2.0) (valueIn hours (120.0 .* minutes))
      assert "should not convert m to m^2" $ isLeft (valueIn (meter .^ 2.0) (2.0 .* meters))
      assert "should not convert meters to seconds" $ isLeft (valueIn seconds (2.0 .* meters))

    test "Addition" do
      equal (Right $ 8.0 .* meter) (3.0 .* meter ⊕ 5.0 .* meter)
      assert "should not add meters and seconds" (isLeft $ 3.0 .* meter ⊕ 5.0 .* second)

    test "Multiplication" do
      equal (15.0 .* meter .^ 2.0) (3.0 .* meter ⊗ 5.0 .* meter)
