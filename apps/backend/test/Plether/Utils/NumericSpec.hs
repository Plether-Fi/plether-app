module Plether.Utils.NumericSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Plether.Utils.Numeric

spec :: Spec
spec = do
  describe "WAD math (10^18 precision)" $ do
    it "wadMul(1 WAD, 1 WAD) = 1 WAD" $
      wadMul wad wad `shouldBe` wad

    it "wadMul(2 WAD, 3 WAD) = 6 WAD" $
      wadMul (2 * wad) (3 * wad) `shouldBe` 6 * wad

    it "wadDiv(6 WAD, 2 WAD) = 3 WAD" $
      wadDiv (6 * wad) (2 * wad) `shouldBe` 3 * wad

    it "wadDiv(1 WAD, 2 WAD) = 0.5 WAD" $
      wadDiv wad (2 * wad) `shouldBe` wad `div` 2

    prop "wadMul is commutative" prop_wadMulCommutative

    prop "wadMul then wadDiv recovers original (within 1)" prop_wadMulDivRoundtrip

    prop "wadDiv then wadMul recovers original (within 1)" prop_wadDivMulRoundtrip

  describe "RAY math (10^27 precision)" $ do
    it "rayMul(1 RAY, 1 RAY) = 1 RAY" $
      rayMul ray ray `shouldBe` ray

    it "rayMul(2 RAY, 3 RAY) = 6 RAY" $
      rayMul (2 * ray) (3 * ray) `shouldBe` 6 * ray

    it "rayDiv(6 RAY, 2 RAY) = 3 RAY" $
      rayDiv (6 * ray) (2 * ray) `shouldBe` 3 * ray

    prop "rayMul is commutative" prop_rayMulCommutative

    prop "rayMul then rayDiv recovers original (within 1)" prop_rayMulDivRoundtrip

  describe "mulDiv" $ do
    it "mulDiv(100, 50, 100) = 50" $
      mulDiv 100 50 100 `shouldBe` 50

    it "mulDiv handles large numbers without overflow" $
      mulDiv (10 ^ (36 :: Integer)) (10 ^ (18 :: Integer)) (10 ^ (18 :: Integer))
        `shouldBe` 10 ^ (36 :: Integer)

    prop "mulDiv(a, b, b) = a when b /= 0" prop_mulDivIdentity

  describe "parseDecimal" $ do
    it "parses '123'" $
      parseDecimal "123" `shouldBe` Just 123

    it "parses '0'" $
      parseDecimal "0" `shouldBe` Just 0

    it "parses with whitespace" $
      parseDecimal "  456  " `shouldBe` Just 456

    it "rejects empty string" $
      parseDecimal "" `shouldBe` Nothing

    it "rejects non-numeric" $
      parseDecimal "abc" `shouldBe` Nothing

    it "rejects negative" $
      parseDecimal "-123" `shouldBe` Nothing

    it "rejects float" $
      parseDecimal "12.34" `shouldBe` Nothing

  describe "formatWei" $ do
    it "formats 1000000 with 6 decimals as '1.0'" $
      formatWei 1000000 6 `shouldBe` "1.0"

    it "formats 1500000 with 6 decimals as '1.5'" $
      formatWei 1500000 6 `shouldBe` "1.5"

    it "formats 1234567 with 6 decimals" $
      formatWei 1234567 6 `shouldBe` "1.234567"

    it "formats 0 with 18 decimals" $
      formatWei 0 18 `shouldBe` "0.0"

    it "formats 1 WAD as '1.0'" $
      formatWei wad 18 `shouldBe` "1.0"

    it "formats fractional amounts" $
      formatWei (wad `div` 3) 18 `shouldBe` "0.333333333333333333"

  describe "weiToEther / etherToWei" $ do
    it "weiToEther(1 WAD) = 1.0" $
      weiToEther wad `shouldBe` 1.0

    it "etherToWei(1.0) = 1 WAD" $
      etherToWei 1.0 `shouldBe` wad

    prop "roundtrip (within floating point tolerance)" prop_weiEtherRoundtrip

prop_wadMulCommutative :: Integer -> Integer -> Bool
prop_wadMulCommutative a b = wadMul a b == wadMul b a

prop_wadMulDivRoundtrip :: Positive Integer -> Positive Integer -> Property
prop_wadMulDivRoundtrip (Positive a') (Positive b') =
  let a = a' * wad
      b = b' * wad
      product' = wadMul a b
      recovered = wadDiv product' b
  in recovered === a

prop_wadDivMulRoundtrip :: Positive Integer -> Positive Integer -> Bool
prop_wadDivMulRoundtrip (Positive a) (Positive b) =
  let quotient = wadDiv a b
      recovered = wadMul quotient b
  in abs (recovered - a) <= b

prop_rayMulCommutative :: Integer -> Integer -> Bool
prop_rayMulCommutative a b = rayMul a b == rayMul b a

prop_rayMulDivRoundtrip :: Positive Integer -> Positive Integer -> Property
prop_rayMulDivRoundtrip (Positive a') (Positive b') =
  let a = a' * ray
      b = b' * ray
      product' = rayMul a b
      recovered = rayDiv product' b
  in recovered === a

prop_mulDivIdentity :: NonZero Integer -> Integer -> Bool
prop_mulDivIdentity (NonZero b) a = mulDiv a b b == a

prop_weiEtherRoundtrip :: Positive Integer -> Bool
prop_weiEtherRoundtrip (Positive wei) =
  let ether = weiToEther wei
      recovered = etherToWei ether
  in abs (recovered - wei) <= 1
