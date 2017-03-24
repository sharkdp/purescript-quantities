module Test.Main where

import Prelude hiding (degree)

import Data.Decimal (fromNumber)
import Data.Either (Either(..), isLeft)
import Data.Tuple (fst, snd)
import Data.Number ((≅))
import Data.Units (unity, (.^), (./), atto, femto, pico, nano, micro, centi,
                   deci, hecto, milli, kilo, mega, giga, tera, peta, exa)
import Data.Units as U
import Data.Units.SI (meter, gram, second, ampere, kelvin, mole, candela)
import Data.Units.SI.Derived (radian, steradian, hertz, newton, pascal, joule,
                              watt, coulomb, volt, farad, ohm, siemens, weber,
                              tesla, henry, lumen, lux, becquerel, gray,
                              sievert, katal)
import Data.Units.SI.Accepted (degree, hectare, liter, tonne, electronvolt)
import Data.Units.Time (hour, minute, day, week, month, year)
import Data.Units.Imperial (inch, mile, foot, yard)
import Data.Units.Bit (bit, byte)
import Data.Quantity (Quantity, (.*), prettyPrint, (⊕), (⊖), (⊗), (⊘),
                      convertTo, asValueIn, pow, scalar, sqrt, derivedUnit,
                      errorMessage, showResult, qNegate)
import Data.Quantity as Q
import Data.Quantity.Math (sin, asin)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Math (pi)

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, assertFalse, equal)

almostEqualNumbers ∷ ∀ e. Number → Number → Test e
almostEqualNumbers x y = do
  if x ≅ y
    then success
    else failure $ "expected " <> show x <> ", got " <> show y

-- | Test if two quantities are almost equal, i.e. if the units match and the
-- | numerical value is approximately the same.
almostEqual ∷ ∀ e. Quantity → Quantity → Test e
almostEqual expected actual = do
  if expected `approximatelyEqual` actual
    then success
    else failure $ "expected " <> show expected <>
                   ", got " <> show actual
  where
    approximatelyEqual = Q.approximatelyEqual tolerance
    tolerance = 1.0e-6

almostEqual' ∷ ∀ e err. Quantity → Either err Quantity → Test e
almostEqual' expected actual =
  case actual of
    Left _ → failure "Conversion error"
    Right actual' → almostEqual expected actual'

main ∷ Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR) Unit
main = runTest do
  let
    meters = meter
    seconds = second
    miles = mile
    minutes = minute
    hours = hour
    inches = inch
    grams = gram

  suite "Data.Units" do
    test "Eq instance" do
      equal meter (meter .^ 1.0)
      equal (meter .^ 2.0) (meter <> meter)
      equal (meter <> second) (second <> meter)
      equal (kilo meter) (kilo meter)
      assertFalse "should check the prefix" $ kilo meter == meter
      assertFalse "should compare prefixes" $ kilo meter == nano meter

    test "Show instance" do
      equal      "meter"
            (show meter)
      equal      "second"
            (show second)
      equal      "meter .^ (2.0)"
           (show (meter .^ (2.0)))
      equal      "meter .^ (-2.0)"
           (show (meter .^ (-2.0)))
      equal      "(meter .^ (2.0) <> second .^ (3.0))"
            (show (meter .^ (2.0) <> second .^ (3.0)))
      equal       "(kilo meter) .^ (2.0)"
            (show ((kilo meter) .^ (2.0)))
      equal       "unity"
            (show unity)
      equal      "((kilo meter) <> meter .^ (-1.0))"
            (show (kilo meter ./ meter))
      equal      "newton"
            (show newton)

    test "Semigroup / Monoid instance" do
      equal meter (meter <> unity)
      equal meter (unity <> meter)
      equal (meter <> meter <> second) (meter <> second <> meter)
      equal (meter <> meter <> second) (second <> meter <> meter)
      equal (kilo meter <> meter <> second) (second <> kilo meter <> meter)

    test "toStandardUnit" do
      let rec = U.toStandardUnit minute
      equal second $ fst rec
      equal 60.0   $ snd rec

    test "toString" do
      equal "m" $ U.toString meter
      equal "m²" $ U.toString (meter <> meter)
      equal "m²" $ U.toString (meter .^ 2.0)
      equal "m³" $ U.toString (meter .^ 3.0)
      equal "m⁴" $ U.toString (meter .^ 4.0)
      equal "m^(8.0)" $ U.toString (meter .^ 8.0)
      equal "m^(-8.0)" $ U.toString (meter .^ -8.0)
      equal "m⁻¹" $ U.toString (meter .^ (-1.0))
      equal "m⁻⁴" $ U.toString (meter .^ (-4.0))
      equal "m²·s" $ U.toString (meter <> meter <> second)
      equal "s²·m" $ U.toString (meter <> second <> second)
      equal "m/s" $ U.toString (meter ./ second)
      equal "m/s²" $ U.toString (meter ./ second .^ 2.0)
      equal "m³/(s²·g)" $ U.toString (meter .^ 3.0 ./ (second .^ 2.0 <> gram))
      equal "J" $ U.toString joule
      equal "°" $ U.toString degree
      equal "s²·km" $ U.toString (kilo meter <> second <> second)
      equal "m·ks·s" $ U.toString (meter <> kilo second <> second)

    test "toString (prefixes)" do
      equal "am" $ U.toString (atto meter)
      equal "fm" $ U.toString (femto meter)
      equal "pm" $ U.toString (pico meter)
      equal "nm" $ U.toString (nano meter)
      equal "µm" $ U.toString (micro meter)
      equal "mm" $ U.toString (milli meter)
      equal "cm" $ U.toString (centi meter)
      equal "dm" $ U.toString (deci meter)
      equal "hm" $ U.toString (hecto meter)
      equal "km" $ U.toString (kilo meter)
      equal "Ms" $ U.toString (mega second)
      equal "Gs" $ U.toString (giga second)
      equal "Ts" $ U.toString (tera second)
      equal "Ps" $ U.toString (peta second)
      equal "Es" $ U.toString (exa second)
      equal "as²" $ U.toString (atto second .^ 2.0)
      equal "Ts²" $ U.toString (tera second .^ 2.0)

    test "power" do
      equal (meter <> meter) (meter .^ 2.0)
      equal unity (meter .^ (-1.0) <> meter)
      equal unity (meter <> meter .^ (-1.0))
      equal (meter ./ second) (meter <> second .^ (-1.0))
      equal (micro (meter .^ 2.0)) (milli meter .^ 2.0)

    test "divideUnits" do
      equal unity (meter <> (unity ./ meter))
      equal unity ((unity ./ meter) <> meter)
      equal meter (meter <> meter ./ meter)
      equal unity (kilo meter ./ kilo meter)
      -- TODO: re-enable this
      -- equal (milli meter) (meter .^ 2.0 ./ kilo meter)



  suite "Data.Quantity" do
    test "Eq instance" do
      equal (3.0 .* meter) ((1.0 + 2.0) .* (meter .^ 1.0))
      assert "should compare units" $ 3.0 .* meter /= 3.0 .* second
      assert "should compare exponents" $ 3.0 .* meter /= 3.0 .* meter .^ 1.1
      assert "should compare values" $ 3.0 .* meter /= 3.01 .* meter

    test "Show instance" do
      equal "(fromString \"-3\") .* meter" $ show (-3.0 .* meter)
      equal "(fromString \"3\") .* (kilo meter)" $ show (3.0 .* kilo meter)
      equal "(fromString \"3.7\") .* (meter .^ (2.0) <> second)" $ show (3.7 .* (meter <> second <> meter))
      equal "(fromString \"2\") .* (tera second) .^ (2.0)" $ show (2.0 .* (tera second .^ 2.0))

    test "prettyPrint" do
      equal "3" $ prettyPrint (scalar 3.0)
      equal "3km" $ prettyPrint (3.0 .* kilo meter)
      equal "3.2m" $ prettyPrint (3.2 .* meter)
      equal "3.71m²·s" $ prettyPrint (3.71 .* (meter <> second <> meter))
      equal "3m/s" $ prettyPrint (3.0 .* meter ./ second)
      equal "-3.12332Ps²" $ prettyPrint ((-3.123321) .* peta second .^ 2.0)
      equal "3km²" $ prettyPrint (3.0 .* kilo meter .^ 2.0)
      equal "3K" $ prettyPrint (3.0 .* kelvin)

    test "derivedUnit" do
      equal meter (Q.derivedUnit (3.0 .* meter))

    test "toStandard" do
      almostEqual (1.0 .* meter ./ second) $
                  Q.toStandard (2362.2047 .* inch ./ minute)

    test "fullSimplify" do
      equal "200" $
        prettyPrint $ Q.fullSimplify (2.0 .* meter ./ centi meter)

      equal "2" $
        prettyPrint $ Q.fullSimplify (2000000.0 .* milli meter ./ kilo meter)

      equal "180°" $
        prettyPrint $ Q.fullSimplify (180.0 .* degree)

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
      assert "should do conversions" $
             upToTenPercent (1000.0 .* meters) (1.0 .* kilo meter)
      assert "should do conversions" $
             upToTenPercent (1000000.0 .* meters) (1000.0 .* kilo meter)
      assert "should do conversions" $
             upToTenPercent (1.0 .* meter) (0.001 .* kilo meters)

    test "scalar" do
      equal (3.0 .* unity) (Q.scalar 3.0)

    test "convert, convertTo" do
      let checkConversion q targetValue targetUnit = do
            let converted = q `convertTo` targetUnit
            case converted of
              Left err → failure $ "Conversion failed: " <> errorMessage err
              Right q' →
                if derivedUnit q' /= targetUnit
                  then failure "Conversion failed: unit /= targetUnit"
                  else
                    case q' `asValueIn` targetUnit of
                      Left err → failure "Conversion failed in this stage?"
                      Right val → almostEqualNumbers targetValue val

      checkConversion (1.0 .* minute)  60.0 seconds
      checkConversion (2.0 .* hours ) 120.0 minutes
      checkConversion (36.0 .* kilo meter ./ hour) 10.0 (meter ./ second)
      checkConversion (12.0 .* inch) 1.0 foot
      checkConversion (1.0 .* foot) 12.0 inch
      checkConversion (3.0 .* foot) 1.0 yard
      checkConversion (36.0 .* inch) 1.0 yard
      checkConversion (2.0 .* foot .^ 2.0) 288.0 (inch .^ 2.0)
      checkConversion (1.0 .* (kilo gram <> meter <> second .^ (-2.0))) 1.0 newton
      checkConversion (1.0 .* newton) 1.0 (kilo gram <> meter <> second .^ (-2.0))
      checkConversion (1.0 .* joule) 1.0 (watt <> second)
      checkConversion (1.0 .* joule) 1.0 (newton <> meter)

      -- the following checks implicitely use `convert`
      almostEqual (120.0 .* seconds) (2.0 .* minutes)
      almostEqual (1.0 .* meter .^ 2.0) (1550.0031 .* inch .^ 2.0)
      almostEqual (50.0 .* meters ./ second) (180000.0 .* meters ./ hour)

    test "asValueIn" do
      equal (Right $ 2.54) ((100.0 .* inches) `asValueIn` meters)
      equal (Right $ 100.0) ((2.54 .* meters) `asValueIn` inches)
      equal (Right $ 120.0) ((2.0 .* hours) `asValueIn` minutes)
      equal (Right $ 2.0) ((120.0 .* minutes) `asValueIn` hours)
      equal (Right $ 2000.0) ((2.0 .* kilo meters) `asValueIn` meters)
      equal (Right $ 2.0) ((2000.0 .* meters) `asValueIn` kilo meters)
      assert "should not convert m to m^2" $
             isLeft ((2.0 .* meters) `asValueIn` (meter .^ 2.0))
      assert "should not convert meters to seconds" $
             isLeft ((2.0 .* meters) `asValueIn` seconds)

    test "Negation" do
      equal ((-8.0) .* meter) (qNegate $ 8.0 .* meter)
      equal (0.0 .* meter) (qNegate $ 0.0 .* meter)

    test "Addition" do
      equal (Right $ 8.0 .* meter) (3.0 .* meter ⊕ 5.0 .* meter)
      equal (Right $ 8.05 .* meter) (8.0 .* meter ⊕ 50.0 .* milli meter)
      assert "should not add meters and seconds" $
             isLeft $ 3.0 .* meter ⊕ 5.0 .* second

    test "Subtraction" do
      equal (Right $ 3.0 .* meter) (5.0 .* meter ⊖ 2.0 .* meter)
      equal (Right $ 994.0 .* meter) (1.0 .* kilo meter ⊖ 6.0 .* meter)
      equal (Right $ 55.0 .* minutes) (1.0 .* hour ⊖ 5.0 .* minutes)
      assert "should not subtract meters from seconds" $
             isLeft $ 3.0 .* second ⊖ 5.0 .* meter

    test "Multiplication" do
      equal (15.0 .* meter .^ 2.0) (3.0 .* meter ⊗ 5.0 .* meter)
      equal (2000.0 .* meter .^ 2.0) (2.0 .* (meter <> kilo meter))
      equal (2000.0 .* meter .^ 2.0) (2.0 .* (kilo meter <> meter))
      equal (4.0 .* joule) (8.0 .* newton ⊗ 0.5 .* meter)

    test "Division" do
      equal (0.5 .* hertz) (scalar 1.0 ⊘ 2.0 .* second)
      equal (0.5 .* kilo hertz) (scalar 1.0 ⊘ 2.0 .* milli second)
      -- TODO: this should also work with `equal` instead of `almostEqual`,
      -- see GitHub issue #12.
      almostEqual (0.5 .* giga hertz) (scalar 1.0 ⊘ 2.0 .* nano second)

    test "pow" do
      let two = fromNumber 2.0
      equal (100.0 .* meter .^ 2.0) ((10.0 .* meter) `pow` two)
      equal (4000000.0 .* meter .^ 2.0) (4.0 .* kilo meter .^ 2.0)
      equal (4.0 .* kilo meter .^ 2.0) ((2.0 .* kilo meter) `pow` two)
      equal (Q.scalar 100.0) ((Q.scalar 10.0) `pow` two)

    test "abs" do
      equal (2.4 .* seconds) (Q.abs (2.4 .* seconds))
      equal (2.4 .* seconds) (Q.abs ((-2.4) .* seconds))
      equal (0.0 .* seconds) (Q.abs (0.0 .* seconds))

  suite "Data.Quantity.Math" do
    test "Functions" do
      almostEqual' (scalar 1.0) (sin (90.0 .* degree))
      almostEqual' (scalar 0.5) (sin (30.0 .* degree))
      almostEqual' (30.0 .* degree) (asin (scalar 0.5))
      almostEqual' (scalar 1.0) (sin ((0.5 * pi) .* radian))

  suite "Consistency checks" do
    test "Data.Units.SI.Derived" do
      -- See: https://en.wikipedia.org/wiki/International_System_of_Units#Derived_units
      equal (1.0 .* (meter .^ 2.0 ./ meter .^ 2.0)) (1.0 .* radian)
      equal (1.0 .* (meter .^ 2.0 ./ meter .^ 2.0)) (1.0 .* steradian)
      equal (1.0 .* second .^ (-1.0)) (1.0 .* hertz)
      equal (1.0 .* (kilo gram <> meter ./ second .^ 2.0)) (1.0 .* newton)
      equal (1.0 .* (newton ./ meter .^ 2.0)) (1.0 .* pascal)
      equal (1.0 .* (newton <> meter)) (1.0 .* joule)
      equal (1.0 .* (joule ./ second)) (1.0 .* watt)
      equal (1.0 .* (ampere <> second)) (1.0 .* coulomb)
      equal (1.0 .* (watt ./ ampere)) (1.0 .* volt)
      equal (1.0 .* (coulomb ./ volt)) (1.0 .* farad)
      equal (1.0 .* (volt ./ ampere)) (1.0 .* ohm)
      equal (1.0 .* (ampere ./ volt)) (1.0 .* siemens)
      equal (1.0 .* (volt <> second)) (1.0 .* weber)
      equal (1.0 .* (weber ./ meter .^ 2.0)) (1.0 .* tesla)
      equal (1.0 .* (weber ./ ampere)) (1.0 .* henry)
      equal (1.0 .* (candela <> steradian)) (1.0 .* lumen)
      equal (1.0 .* (lumen ./ meter .^ 2.0)) (1.0 .* lux)
      equal (1.0 .* (second .^ (-1.0))) (1.0 .* becquerel)
      equal (1.0 .* (joule ./ kilo gram)) (1.0 .* gray)
      equal (1.0 .* (joule ./ kilo gram)) (1.0 .* sievert)
      equal (1.0 .* (mole ./ second)) (1.0 .* katal)

    test "Data.Units.SI.Accepted" do
      -- See: https://en.wikipedia.org/wiki/Non-SI_units_mentioned_in_the_SI
      equal ((pi / 180.0) .* radian) (1.0 .* degree)
      equal (1.0 .* (hecto meter) .^ 2.0) (1.0 .* hectare)
      almostEqual (1.0 .* (deci meter) .^ 3.0) (1.0 .* liter)
      equal (1000.0 .* kilo gram) (1.0 .* tonne)
      almostEqual (1.602176e-19 .* joule) (1.0 .* electronvolt)

    test "Data.Units.Time" do
      equal (60.0 .* second)  (1.0 .* minute)
      equal (60.0 .* minute)  (1.0 .* hour)
      equal (7.0 .* day)      (1.0 .* week)
      equal (24.0 .* hour)    (1.0 .* day)
      equal (730.0 .* hour)   (1.0 .* month)
      equal (365.0 .* day)    (1.0 .* year)
      equal (12.0 .* month)   (1.0 .* year)

  suite "Integration" do
    let testExample nr output input =
          test ("Example " <> show nr) $
            equal output (showResult input)

    testExample 1 "2.5min" $
      2.0 .* minutes ⊕ 30.0 .* seconds

    testExample 2 "36km/h" $
      (10.0 .* meters ./ second) `convertTo` (kilo meters ./ hour)

    testExample 3 "37.9984m/s" $
      (85.0 .* miles ./ hour) `convertTo` (meters ./ second)

    testExample 4 "Cannot unify unit 'mi' (SI: 'm')\n        with unit 'g²' (SI: 'g²')" $
      (10.0 .* miles) `convertTo` (grams .^ 2.0)

    testExample 5 "1" $
      sin (90.0 .* degree)

    let filesize = 2.7 .* giga byte
        speed = 6.0 .* mega bit ./ second
        time = (filesize ⊘ speed) `convertTo` minute
    testExample 6 "60min" time

    let g = 9.81 .* meters ./ second .^ 2.0
        length = 20.0 .* centi meter
        period = scalar (2.0 * pi) ⊗ sqrt (length ⊘ g)
    testExample 7 "897.14ms"
      (period `convertTo` milli second)
