module Test.Main where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Tuple (fst, snd)

import Data.BaseUnit as B
import Data.DerivedUnit (meter, meters, second, seconds, minute, minutes, hour,
                         hours, inch, inches, miles, grams, unity, (.^), (./))
import Data.DerivedUnit as D
import Data.Quantity (Quantity, (.*), (⊕), (⊗), convertTo, asValueIn, pow)
import Data.Quantity as Q

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, assertFalse, equal)

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

  suite "BaseUnit" do
    test "isStandardUnit" do
      assert "meter is a standard unit" $ B.isStandardUnit B.meter
      assert "inch is a non-standard unit" $ not (B.isStandardUnit B.inch)

    test "toStandardUnit" do
      equal B.meter (B.toStandardUnit B.inch)
      equal B.second (B.toStandardUnit B.hour)

    test "conversionFactor" do
      equal    1.0 (B.conversionFactor B.second)
      equal   60.0 (B.conversionFactor B.minute)
      equal 3600.0 (B.conversionFactor B.hour)



  suite "DerivedUnit" do
    test "Eq instance" do
      equal meter (meter .^ 1.0)
      equal (meter .^ 2.0) (meter <> meter)
      equal (meter <> second) (second <> meter)

    test "Show instance" do
      equal      "meter"
            (show meter)
      equal      "meter .^ 2.0"
           (show (meter .^ 2.0))
      equal       "meter .^ 2.0 <> second .^ 3.0"
            (show (meter .^ 2.0 <> second .^ 3.0))

    test "Semigroup / Monoid instance" do
      equal meter (meter <> unity)
      equal meter (unity <> meter)
      equal (meter <> meter <> second) (meter <> second <> meter)
      equal (meter <> meter <> second) (second <> meter <> meter)

    test "toStandardUnit" do
      let rec = D.toStandardUnit minute
      equal second $ fst rec
      equal 60.0   $ snd rec

    test "toString" do
      equal "m" $ D.toString meter
      equal "m²" $ D.toString (meter <> meter)
      equal "m²" $ D.toString (meter .^ 2.0)
      equal "m³" $ D.toString (meter .^ 3.0)
      equal "m^(4.0)" $ D.toString (meter .^ 4.0)
      equal "m^(-1.0)" $ D.toString (meter .^ (-1.0))
      equal "m²·s" $ D.toString (meter <> meter <> second)
      equal "m·s²" $ D.toString (meter <> second <> second)

    test "power" do
      equal (meter <> meter) (meter .^ 2.0)
      equal unity (meter .^ (-1.0) <> meter)
      equal unity (meter <> meter .^ (-1.0))
      equal (meter ./ second) (meter <> second .^ (-1.0))

    test "divideUnits" do
      equal unity (meter <> (unity ./ meter))
      equal unity ((unity ./ meter) <> meter)
      equal meter (meter <> meter ./ meter)



  suite "Quantity" do
    test "Eq instance" do
      equal (3.0 .* meter) ((1.0 + 2.0) .* (meter .^ 1.0))
      assert "should compare units" $ 3.0 .* meter /= 3.0 .* second
      assert "should compare exponents" $ 3.0 .* meter /= 3.0 .* meter .^ 1.1
      assert "should compare values" $ 3.0 .* meter /= 3.01 .* meter

    test "Show instance" do
      equal "3.0m" $ show (3.0 .* meter)
      equal "3.0m²·s" $ show (3.0 .* (meter <> second <> meter))

    test "derivedUnit" do
      equal meter (Q.derivedUnit (3.0 .* meter))

    test "toStandard" do
      almostEqual (1.0 .* meter ./ second) $
                  Q.toStandard (2362.2047 .* inch ./ minute)

    test "approximatelyEqual" do
      let upToTenPercent = Q.approximatelyEqual 0.1
      assert "should tolerate small differences" $
             upToTenPercent (10.0 .* meters) (10.4 .* meters)
      assert "should not depend on scale" $
             upToTenPercent (10000.0 .* meters) (10400.0 .* meters)
      assertFalse "should not tolerate large differences" $
             upToTenPercent (10.0 .* meters) (11.4 .* meters)
      assertFalse "should check units" $
             upToTenPercent (10.0 .* meters) (10.0 .* seconds)

    test "scalar" do
      equal (3.0 .* unity) (Q.scalar 3.0)

    test "convert, convertTo" do
      equal (Right $ 60.0 .* seconds) ((1.0 .* minute) `convertTo` seconds)

      -- the following checks implicitely use `convert`
      almostEqual (120.0 .* seconds) (2.0 .* minutes)
      almostEqual (1.0 .* meter .^ 2.0) (1550.0031 .* inch .^ 2.0)
      almostEqual (50.0 .* meters ./ second) (180000.0 .* meters ./ hour)

    test "asValueIn" do
      equal (Right $ 2.54) ((100.0 .* inches) `asValueIn` meters)
      equal (Right $ 100.0) ((2.54 .* meters) `asValueIn` inches)
      equal (Right $ 120.0) ((2.0 .* hours) `asValueIn` minutes)
      equal (Right $ 2.0) ((120.0 .* minutes) `asValueIn` hours)
      assert "should not convert m to m^2" $
             isLeft ((2.0 .* meters) `asValueIn` (meter .^ 2.0))
      assert "should not convert meters to seconds" $
             isLeft ((2.0 .* meters) `asValueIn` seconds)

    test "Addition" do
      equal (Right $ 8.0 .* meter) (3.0 .* meter ⊕ 5.0 .* meter)
      assert "should not add meters and seconds" $
             isLeft $ 3.0 .* meter ⊕ 5.0 .* second

    test "Multiplication" do
      equal (15.0 .* meter .^ 2.0) (3.0 .* meter ⊗ 5.0 .* meter)

    test "pow" do
      equal (100.0 .* meter .^ 2.0) ((10.0 .* meter) `pow` 2.0)
      equal (Q.scalar 100.0) ((Q.scalar 10.0) `pow` 2.0)

    test "abs" do
      equal (2.4 .* seconds) (Q.abs (2.4 .* seconds))
      equal (2.4 .* seconds) (Q.abs ((-2.4) .* seconds))

  suite "Integration" do
    test "Example 1" do
      equal (Right $ 2.5 .* minutes) (2.0 .* minutes ⊕ 30.0 .* seconds)

    test "Example 2" do
      equal (Right 37.9984) $
            (85.0 .* miles ./ hour) `asValueIn` (meters ./ second)

    test "Example 3" do
      assert "should fail with error" $
             isLeft ((10.0 .* miles) `asValueIn` (grams .^ 2.0))
