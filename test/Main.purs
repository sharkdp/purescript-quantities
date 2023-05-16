module Test.Main (main) where

import Prelude hiding (degree, min, max)

import Data.Decimal (fromNumber)
import Data.Either (Either(..), isLeft, isRight)
import Data.Tuple (fst, snd)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Number.Approximate ((≅))
import Data.Units (unity, (.^), (./), atto, femto, pico, nano, micro, centi,
                   deci, hecto, milli, kilo, mega, giga, tera, peta, exa,
                   removePrefix, kibi, mebi, gibi, exbi, zepto, yocto, ronto,
                   quecto, quetta, ronna, yotta, zetta)
import Data.Units as U
import Data.Units.SI (meter, gram, second, ampere, kelvin, mole, candela)
import Data.Units.SI.Derived (radian, steradian, hertz, newton, pascal, joule,
                              watt, coulomb, volt, farad, ohm, siemens, weber,
                              tesla, henry, lumen, lux, becquerel, gray,
                              sievert, katal)
import Data.Units.SI.Accepted (degree, hectare, liter, tonne, electronvolt,
                               bel, astronomicalUnit, bar, angstrom, barn)
import Data.Units.Time (hour, minute, day, week, month, year, julianYear)
import Data.Units.Imperial (inch, mile, foot, yard, thou, furlong)
import Data.Units.USCustomary (gallon, pint, cup, tablespoon, teaspoon,
                               fluidounce, hogshead, rod)
import Data.Units.Nautical(knot, nauticalMile)
import Data.Units.PartsPerX(percent, partsPerMillion, partsPerBillion, partsPerTrillion, partsPerQuadrillion)
import Data.Units.CGS (gauss)
import Data.Units.Astronomical (parsec, lightyear)
import Data.Units.Misc (calorie, btu, lbf, ozf, rpm, fortnight, mmHg, psi, atm)
import Data.Units.Bit (bit, byte)
import Data.Quantity (Quantity, (.*), prettyPrint, (⊕), (⊖), (⊗), (⊘),
                      convertTo, asValueIn, pow, scalar, sqrt, derivedUnit,
                      errorMessage, showResult, qNegate, isFinite,
                      ConversionError(..))
import Data.Quantity as Q
import Data.Quantity.Math (sin, asin, pi, modulo, max, min, mean, atan2)

import Effect (Effect)

import Data.Number as Number

import Test.Unit (Test, suite, test, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert, assertFalse, equal)

almostEqualNumbers ∷ Number → Number → Test
almostEqualNumbers x y =
  if x ≅ y
    then success
    else failure $ "expected " <> show x <> ", got " <> show y

-- | Test if two quantities are almost equal, i.e. if the units match and the
-- | numerical value is approximately the same.
almostEqual ∷ Quantity → Quantity → Test
almostEqual expected actual =
  if expected `approximatelyEqual` actual
    then success
    else failure $ "expected " <> show expected <>
                   ", got " <> show actual
  where
    approximatelyEqual = Q.approximatelyEqual tolerance
    tolerance = 1.0e-6

almostEqual' ∷ ∀ err. Quantity → Either err Quantity → Test
almostEqual' expected actual =
  case actual of
    Left _ → failure "Conversion error"
    Right actual' → almostEqual expected actual'

main ∷ Effect Unit
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
      equal (second <> meter) (second <> meter)
      equal (kilo meter) (kilo meter)
      equal (kilo (mega gram)) (giga gram)
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
      equal (second <> meter <> meter) (second <> meter <> meter)
      equal (second <> kilo meter <> meter) (second <> kilo meter <> meter)

    test "toStandardUnit" do
      let rec = U.toStandardUnit minute
      equal second            $ fst rec
      equal (fromNumber 60.0) $ snd rec

    test "toString" do
      equal "m" $ U.toString meter
      equal "m²" $ U.toString (meter <> meter)
      equal "m²" $ U.toString (meter .^ 2.0)
      equal "m³" $ U.toString (meter .^ 3.0)
      equal "m⁴" $ U.toString (meter .^ 4.0)
      equal "m^(8.0)" $ U.toString (meter .^ 8.0)
      equal "m^(-8.0)" $ U.toString (meter .^ -8.0)
      equal "m⁻¹" $ U.toString (meter .^ -1.0)
      equal "m⁻⁴" $ U.toString (meter .^ -4.0)
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
      equal "qm" $ U.toString (quecto meter)
      equal "rm" $ U.toString (ronto meter)
      equal "ym" $ U.toString (yocto meter)
      equal "zm" $ U.toString (zepto meter)
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
      equal "Zs" $ U.toString (zetta second)
      equal "Ys" $ U.toString (yotta second)
      equal "Rs" $ U.toString (ronna second)
      equal "Qs" $ U.toString (quetta second)
      equal "as²" $ U.toString (atto second .^ 2.0)
      equal "Ts²" $ U.toString (tera second .^ 2.0)
      equal "KiB" $ U.toString (kibi byte)
      equal "MiB" $ U.toString (mebi byte)
      equal "GiB" $ U.toString (gibi byte)
      equal "GiB²" $ U.toString (gibi byte .^ 2.0)

    test "power" do
      equal (meter <> meter) (meter .^ 2.0)
      equal unity (meter .^ -1.0 <> meter)
      equal unity (meter <> meter .^ -1.0)
      equal (meter ./ second) (meter <> second .^ -1.0)
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
      equal (3.0 .* meter) (3.0 .* (meter .^ 1.0))
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
      equal "3 km" $ prettyPrint (3.0 .* kilo meter)
      equal "3.2 m" $ prettyPrint (3.2 .* meter)
      equal "3.71 m²·s" $ prettyPrint (3.71 .* (meter <> second <> meter))
      equal "3 m/s" $ prettyPrint (3.0 .* meter ./ second)
      equal "-3.12332 Ps²" $ prettyPrint (-3.123321 .* peta second .^ 2.0)
      equal "3 km²" $ prettyPrint (3.0 .* kilo meter .^ 2.0)
      equal "3 K" $ prettyPrint (3.0 .* kelvin)

    test "SI-compliant pretty printing" do
      --  See: https://en.wikipedia.org/wiki/International_System_of_Units
      --        -> Unit symbols and the value of quantities

      -- The value of a quantity is written as a number followed by a space
      -- (representing a multiplication sign) and a unit symbol; e.g., 2.21 kg,
      -- 7.3×10² m², 22 K.
      equal "2.21 kg" $ prettyPrint (2.21 .* kilo gram)
      equal "22 K" $ prettyPrint (22.0 .* kelvin)

      -- Exceptions are the symbols for plane angular degrees, minutes, and
      -- seconds (°, ′, and ″), which are placed immediately after the
      -- number with no intervening space.
      equal "90°" $ prettyPrint (90.0 .* degree)

      -- A prefix is part of the unit, and its symbol is prepended to the
      -- unit symbol without a separator (e.g., k in km, M in MPa, G in GHz).
      -- Compound prefixes are not allowed.
      equal "1 MPa" $ prettyPrint (1.0 .* mega pascal)

      -- Symbols for derived units formed by multiplication are joined with a
      -- centre dot (·) or a non-breaking space; e.g., N·m or N m.
      equal "1 N·m" $ prettyPrint (1.0 .* (newton <> meter))

      -- Symbols for derived units formed by division are joined with a solidus
      -- (/), or given as a negative exponent. E.g., the "metre per second" can
      -- be written m/s, m s^(−1), m·s^(−1), or m/s. Only one solidus should
      -- be used; e.g., kg/(m·s2) and kg·m^(−1)·s^(−2) are acceptable, but
      -- kg/m/s² is ambiguous and unacceptable.
      equal "1 m/s" $ prettyPrint (1.0 .* (meter ./ second))
      equal "1 kg/(s²·m)" $
          prettyPrint (1.0 .* (kilo gram ./ (meter <> second .^ 2.0)))

    test "derivedUnit" do
      equal meter (Q.derivedUnit (3.0 .* meter))

    test "toStandard" do
      almostEqual (1.0 .* meter ./ second) $
                  Q.toStandard (2362.2047 .* inch ./ minute)

    test "removePrefix" do
      equal (meter <> second) (removePrefix (kilo meter <> nano second))
      equal (byte <> second .^ 2.0) (removePrefix (mebi byte <> nano second .^ 2.0))

    test "simplify" do
      equal "m" $
        U.toString $ U.simplify meter

      equal "m²·s" $
        U.toString $ U.simplify (meter <> second <> meter)

      equal "µs·s·m·cm" $
        U.toString $ U.simplify (micro second <> meter <> second <> centi meter)

      equal "s/m" $
        U.toString $ U.simplify (meter <> second <> meter .^ -2.0)

    test "fullSimplify" do
      equal "200" $
        prettyPrint $ Q.fullSimplify (2.0 .* meter ./ centi meter)

      equal "2" $
        prettyPrint $ Q.fullSimplify (2000000.0 .* milli meter ./ kilo meter)

      equal "180°" $
        prettyPrint $ Q.fullSimplify (180.0 .* degree)

      equal "10 mrad" $
        prettyPrint $ Q.fullSimplify (10.0 .* milli radian)

      equal "10 m²·s" $
        prettyPrint $ Q.fullSimplify (10.0 .* (meter <> second <> meter))

      equal "0.5 g" $
        prettyPrint $ Q.fullSimplify (50.0 .* (gram <> centi meter ./ meter))

      equal "18000 Mbit" $
        prettyPrint $ Q.fullSimplify (5.0 .* (mega bit ./ second <> hour))

      equal "500 cm²" $
        prettyPrint $ Q.fullSimplify (5.0 .* (centi meter <> meter))

      equal "0.05 m²" $
        prettyPrint $ Q.fullSimplify (5.0 .* (meter <> centi meter))

      equal "12.7 cm²" $
        prettyPrint $ Q.fullSimplify (5.0 .* (centi meter <> inch))

      equal "1 s⁻²" $
        prettyPrint $ Q.fullSimplify (1.0 .* (hertz ./ second))

      equal "1 s³·m" $
        prettyPrint $ Q.fullSimplify (1.0 .* (second <> meter ./ hertz .^ 2.0))

      equal "5 g" $
        prettyPrint $ Q.fullSimplify (5.0 .* (gram <> hertz <> second))

      equal "5 g·s" $
        prettyPrint $ Q.fullSimplify (5.0 .* (gram <> hertz <> second .^ 2.0))

      equal "0.005 m²" $
        prettyPrint $ Q.fullSimplify (5.0 .* (meter .^ -1.0 <> liter))

      equal "0.005 m²" $
        prettyPrint $ Q.fullSimplify (5.0 .* (liter ./ meter))

      equal "0.464515 m" $
        prettyPrint $ Q.fullSimplify (5.0 .* (foot .^ 2.0 ./ meter))

      equal "231 in²" $
        prettyPrint $ Q.fullSimplify (1.0 .* (gallon ./ inch))

      equal "231 in" $
        prettyPrint $ Q.fullSimplify (1.0 .* (gallon ./ inch .^ 2.0))

      equal "0.3048 m²" $
        prettyPrint $ Q.fullSimplify (1.0 .* (foot <> meter))

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
                      Left _ → failure "Conversion failed in this stage?"
                      Right val → almostEqualNumbers targetValue val

      checkConversion (1.0 .* minute)  60.0 seconds
      checkConversion (2.0 .* hours ) 120.0 minutes
      checkConversion (36.0 .* kilo meter ./ hour) 10.0 (meter ./ second)
      checkConversion (12.0 .* inch) 1.0 foot
      checkConversion (1.0 .* foot) 12.0 inch
      checkConversion (3.0 .* foot) 1.0 yard
      checkConversion (36.0 .* inch) 1.0 yard
      checkConversion (2.0 .* foot .^ 2.0) 288.0 (inch .^ 2.0)
      checkConversion (0.001 .* inch) 1.0 thou
      checkConversion (1.0 .* (kilo gram <> meter <> second .^ -2.0)) 1.0 newton
      checkConversion (1.0 .* newton) 1.0 (kilo gram <> meter <> second .^ -2.0)
      checkConversion (1.0 .* joule) 1.0 (watt <> second)
      checkConversion (1.0 .* joule) 1.0 (newton <> meter)

      -- the following checks implicitely use `convert`
      almostEqual (120.0 .* seconds) (2.0 .* minutes)
      almostEqual (1.0 .* meter .^ 2.0) (1550.0031 .* inch .^ 2.0)
      almostEqual (50.0 .* meters ./ second) (180000.0 .* meters ./ hour)

    test "asValueIn" do
      equal (Right 2.54) ((100.0 .* inches) `asValueIn` meters)
      equal (Right 100.0) ((2.54 .* meters) `asValueIn` inches)
      equal (Right 120.0) ((2.0 .* hours) `asValueIn` minutes)
      equal (Right 2.0) ((120.0 .* minutes) `asValueIn` hours)
      equal (Right 2000.0) ((2.0 .* kilo meters) `asValueIn` meters)
      equal (Right 2.0) ((2000.0 .* meters) `asValueIn` kilo meters)
      assert "should not convert m to m^2" $
             isLeft ((2.0 .* meters) `asValueIn` (meter .^ 2.0))
      assert "should not convert meters to seconds" $
             isLeft ((2.0 .* meters) `asValueIn` seconds)

    test "isFinite" do
      assert "should be finite" $
        isFinite (4.0 .* meter)
      assertFalse "should be infinite" $
        isFinite ((1.0 .* meter) ⊘ (0.0 .* second))
      assertFalse "should be NaN" $
        isFinite (sqrt (scalar (-1.0)))
      equal (Right false) (isFinite <$> (asin (scalar 2.0)))

    test "Negation" do
      equal (-8.0 .* meter) (qNegate $ 8.0 .* meter)
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

    test "Zero" do
      -- 0 m == 0 s
      equal (zero .* meter) (zero .* second)

      assert "should be able to convert zero to anything" $
        isRight $ (zero .* unity) `convertTo` meter

      assert "should be able to convert zero to anything" $
        isRight $ (zero .* second) `convertTo` meter

      -- should be able to add 0 to anything
      equal (Right $ 5.0 .* meter) (5.0 .* meter ⊕ 0.0 .* unity)
      equal (Right $ 5.0 .* meter) (0.0 .* unity ⊕ 5.0 .* meter)
      equal (Right $ 5.0 .* meter) (5.0 .* meter ⊕ 0.0 .* seconds)
      equal (Right $ 5.0 .* meter) (0.0 .* seconds ⊕ 5.0 .* meter)

      -- same for subtraction
      equal (Right $ 5.0 .* meter) (5.0 .* meter ⊖ 0.0 .* unity)
      equal (Right $ -5.0 .* meter) (0.0 .* unity ⊖ 5.0 .* meter)
      equal (Right $ 5.0 .* meter) (5.0 .* meter ⊖ 0.0 .* seconds)
      equal (Right $ -5.0 .* meter) (0.0 .* seconds ⊖ 5.0 .* meter)

      -- sin(0 m) == 0
      equal (Right $ 0.0 .* unity) (sin (0.0 .* meter))

    test "Multiplication" do
      equal (15.0 .* meter .^ 2.0) (3.0 .* meter ⊗ 5.0 .* meter)
      equal (2000.0 .* meter .^ 2.0) (2.0 .* (meter <> kilo meter))
      equal (2000.0 .* meter .^ 2.0) (2.0 .* (kilo meter <> meter))
      equal (4.0 .* joule) (8.0 .* newton ⊗ 0.5 .* meter)

    test "Division" do
      equal (0.5 .* hertz) (scalar 1.0 ⊘ 2.0 .* second)
      equal (0.5 .* kilo hertz) (scalar 1.0 ⊘ 2.0 .* milli second)
      equal (0.5 .* mega hertz) (scalar 1.0 ⊘ 2.0 .* micro second)
      equal (0.5 .* giga hertz) (scalar 1.0 ⊘ 2.0 .* nano second)
      equal (0.5 .* tera hertz) (scalar 1.0 ⊘ 2.0 .* pico second)
      equal (0.5 .* peta hertz) (scalar 1.0 ⊘ 2.0 .* femto second)
      equal (0.5 .* exa  hertz) (scalar 1.0 ⊘ 2.0 .* atto second)
      equal (0.5 .* zetta hertz) (scalar 1.0 ⊘ 2.0 .* zepto second)
      equal (0.5 .* yotta hertz) (scalar 1.0 ⊘ 2.0 .* yocto second)
      equal (0.5 .* ronna hertz) (scalar 1.0 ⊘ 2.0 .* ronto second)
      equal (0.5 .* quetta hertz) (scalar 1.0 ⊘ 2.0 .* quecto second)

    test "pow" do
      let two = fromNumber 2.0
      equal (100.0 .* meter .^ 2.0) ((10.0 .* meter) `pow` two)
      equal (4000000.0 .* meter .^ 2.0) (4.0 .* kilo meter .^ 2.0)
      equal (4.0 .* kilo meter .^ 2.0) ((2.0 .* kilo meter) `pow` two)
      equal (Q.scalar 100.0) ((Q.scalar 10.0) `pow` two)

    test "abs" do
      equal (2.4 .* seconds) (Q.abs (2.4 .* seconds))
      equal (2.4 .* seconds) (Q.abs (-2.4 .* seconds))
      equal (0.0 .* seconds) (Q.abs (0.0 .* seconds))

    test "Prefixes" do
      equal (1024.0 .* byte) (1.0 .* kibi byte)
      equal ((1024.0 `Number.pow` 3.0) .* byte) (1.0 .* gibi byte)
      equal (kibi byte <> mega meter) (mega byte <> kibi meter)
      equal (giga byte <> kibi meter <> exbi second <> mega gram)
            (mega byte <> exbi meter <> kibi second <> giga gram)

  suite "Data.Quantity.Math" do
    test "Functions" do
      almostEqual' (scalar 1.0) (sin (90.0 .* degree))
      almostEqual' (scalar 0.5) (sin (30.0 .* degree))
      almostEqual' (30.0 .* degree) (asin (scalar 0.5))
      almostEqual' (scalar 1.0) (sin ((0.5 * Number.pi) .* radian))

    test "modulo" do
      equal (Right $ scalar 2.0) ((scalar 5.0) `modulo` (scalar 3.0))
      equal (Right $ scalar 3.0) ((scalar (-1.0)) `modulo` (scalar 4.0))
      equal (Right $ 3.0 .* centi meter) ((8.0 .* centi meter) `modulo` (5.0 .* centi meter))
      equal (Right $ 35.0 .* centi meter) ((235.0 .* centi meter) `modulo` (1.0 .* meter))
      equal (Right $ 0.04 .* meter) ((2.0 .* meter) `modulo` (7.0 .* centi meter))
      equal (Left (ConversionError second meter)) ((8.0 .* meter) `modulo` (5.0 .* seconds))

    test "atan2" do
      almostEqual' (scalar (Number.pi / 2.0)) ((scalar 10.0) `atan2` (scalar 0.0))
      almostEqual' (scalar (Number.pi / 4.0)) ((100.0 .* centi meter) `atan2` (1.0 .* meter))
      equal (Left (ConversionError meter second)) ((1.0 .* second) `atan2` (2.0 .* meter))

    let qs1 = NonEmptyList $ scalar (-3.0) :| scalar 4.0 : scalar 2.0 : Nil
    let qs2 = NonEmptyList $  (300.0 .* centi meter)
                           :| (0.0254 .* meter)
                           :  (-1.0 .* inch)
                           :   Nil
    let qs3 = NonEmptyList $ (4.2 .* second) :| Nil

    test "max" do
      equal (Right $ scalar 4.0) (max qs1)
      equal (Right $ 300.0 .* centi meter) (max qs2)
      equal (Right $ 4.2 .* second) (max qs3)

    test "min" do
      equal (Right $ scalar (-3.0)) (min qs1)
      equal (Right $ -1.0 .* inch) (min qs2)
      equal (Right $ 4.2 .* second) (min qs3)

    test "mean" do
      equal (Right $ scalar 1.0) (mean qs1)
      equal (Right $ 1.0 .* meter) (mean qs2)
      equal (Right $ 4.2 .* second) (mean qs3)

  suite "Consistency checks" do
    test "Data.Units.SI.Derived" do
      -- See: https://en.wikipedia.org/wiki/International_System_of_Units#Derived_units
      equal (1.0 .* (meter .^ 2.0 ./ meter .^ 2.0)) (1.0 .* radian)
      equal (1.0 .* (meter .^ 2.0 ./ meter .^ 2.0)) (1.0 .* steradian)
      equal (1.0 .* second .^ -1.0) (1.0 .* hertz)
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
      equal (1.0 .* (second .^ -1.0)) (1.0 .* becquerel)
      equal (1.0 .* (joule ./ kilo gram)) (1.0 .* gray)
      equal (1.0 .* (joule ./ kilo gram)) (1.0 .* sievert)
      equal (1.0 .* (mole ./ second)) (1.0 .* katal)

    test "Data.Units.SI.Accepted" do
      -- See: https://en.wikipedia.org/wiki/Non-SI_units_mentioned_in_the_SI
      equal ((Number.pi / 180.0) .* radian) (1.0 .* degree)
      equal (1.0 .* hecto meter .^ 2.0) (1.0 .* hectare)
      almostEqual (1.0 .* deci meter .^ 3.0) (1.0 .* liter)
      equal (1000.0 .* kilo gram) (1.0 .* tonne)
      almostEqual (1.602176e-19 .* joule) (1.0 .* electronvolt)
      equal (0.1 .* bel) (1.0 .* deci bel)
      equal (149597870700.0 .* meter) (1.0 .* astronomicalUnit)
      equal (1.0e5 .* pascal) (1.0 .* bar)
      equal (100.0 .* pico meter) (1.0 .* angstrom)
      equal (100.0 .* femto meter .^ 2.0) (1.0 .* barn)

    test "Data.Units.Time" do
      equal (60.0 .* second)  (1.0 .* minute)
      equal (60.0 .* minute)  (1.0 .* hour)
      equal (7.0 .* day)      (1.0 .* week)
      equal (24.0 .* hour)    (1.0 .* day)
      equal (730.485 .* hour) (1.0 .* month)
      equal (365.2425 .* day) (1.0 .* year)
      equal (12.0 .* month)   (1.0 .* year)
      equal (365.25 .* day)   (1.0 .* julianYear)

    test "Data.Units.USCustomary" do
      equal (231.0 .* inch .^ 3.0) ( 1.0 .* gallon)
      equal (1.0 .* gallon)        ( 8.0 .* pint)
      equal (1.0 .* pint)          ( 2.0 .* cup)
      equal (1.0 .* cup)           (16.0 .* tablespoon)
      equal (1.0 .* tablespoon)    ( 3.0 .* teaspoon)
      equal (2.0 .* tablespoon)    ( 1.0 .* fluidounce)
      equal (63.0 .* gallon)       ( 1.0 .* hogshead)
      equal (16.5 .* foot)         ( 1.0 .* rod)

    test "Data.Units.CGS" do
      equal (100.0 .* micro tesla) (1.0 .* gauss)

    test "Data.Units.Imperial" do
      equal (220.0 .* yard) (1.0 .* furlong)

    test "Data.Units.Astronomical" do
      almostEqual (206264.806247096 .* astronomicalUnit) (1.0 .* parsec)
      almostEqual (63241.077 .* astronomicalUnit) (1.0 .* lightyear)
      almostEqual (0.30660139 .* parsec) (1.0 .* lightyear)

    test "Data.Units.Nautical" do
      almostEqual (1.0 .* (nauticalMile ./ hour)) (1.0 .* knot)

    test "Data.Units.PartsPerX" do
      equal (3.0 .* percent) (3e-2 .* unity)
      equal (400.0 .* partsPerMillion) (400e-6 .* unity)
      equal (8.7 .* partsPerBillion) (8.7e-9 .* unity)
      equal (22.9 .* partsPerTrillion) (22.9e-12 .* unity)
      equal (10.24 .* partsPerQuadrillion) (10.24e-15 .* unity)
      equal (400.0 .* (partsPerMillion <> (meter .^ 3.0))) (0.4 .* liter)

    test "Data.Units.Misc" do
      equal (4.184 .* joule) (1.0 .* calorie)
      equal (1055.05585262 .* joule) (1.0 .* btu)
      equal (4.448222 .* newton) (1.0 .* lbf)
      equal ((4.448222 / 16.0) .* newton) (1.0 .* ozf)
      almostEqual (1.0 .* hertz) (60.0 .* rpm)
      equal (2.0 .* week) (1.0 .* fortnight)
      equal (133.322387415 .* pascal) (1.0 .* mmHg)
      equal (6.894757 .* kilo pascal) (1.0 .* psi)
      equal (101325.0 .* pascal) (1.0 .* atm)

  suite "Integration" do
    let testExample nr output input =
          test ("Example " <> show nr) $
            equal output (showResult input)

    testExample 1 "2.5 min" $
      2.0 .* minutes ⊕ 30.0 .* seconds

    testExample 2 "36 km/h" $
      (10.0 .* meters ./ second) `convertTo` (kilo meters ./ hour)

    testExample 3 "37.9984 m/s" $
      (85.0 .* miles ./ hour) `convertTo` (meters ./ second)

    testExample 4 "Cannot convert unit 'mi' (SI: 'm')\n            to unit 'g²' (SI: 'g²')" $
      (10.0 .* miles) `convertTo` (grams .^ 2.0)

    testExample 5 "1" $
      sin (90.0 .* degree)

    let filesize = 2.7 .* giga byte
        speed = 6.0 .* mega bit ./ second
        time = (filesize ⊘ speed) `convertTo` minute
    testExample 6 "60 min" time

    let g = 9.81 .* meter ./ second .^ 2.0
        length = 20.0 .* centi meter
        period = scalar 2.0 ⊗ pi ⊗ sqrt (length ⊘ g)
    testExample 7 "897.14 ms"
      (period `convertTo` milli second)
