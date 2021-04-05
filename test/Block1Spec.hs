module Block1Spec
 ( spec
 ) where

import Test.Hspec ( Spec, describe, it, shouldBe )

import Block1

spec :: Spec
spec = do
  it "run f1_1" $ do
    f1_1 1 `shouldBe` 1
    f1_1 (-101) `shouldBe` (-101)
  it "run f2_1" $ do
    f2_1 0 `shouldBe` False
    f2_1 (-1) `shouldBe` False
    f2_1 101 `shouldBe` True
  it "run f3_1" $ do
    f3_1 False False  `shouldBe` 0
    f3_1 False True `shouldBe` 1
    f3_1 True False `shouldBe` 1
    f3_1 True True `shouldBe` 2
  it "run f4_1" $ do
    f4_1 2 `shouldBe` 1
    f4_1 5 `shouldBe` 1
    f4_1 (-6) `shouldBe` 6
  it "run f5_1" $ do
    f5_1 5 `shouldBe` 6
    f5_1 6 `shouldBe` 28
    f5_1 10 `shouldBe` 28
  it "run f6_1" $ do
    f6_1 5 `shouldBe` 6
    f6_1 6 `shouldBe` 28
    f6_1 10 `shouldBe` 28
  it "run f7_1" $ do
    f7_1 0 0 `shouldBe` 1
    f7_1 3 4 `shouldBe` 125
  it "run f8_1" $ do
    f8_1 1 2 `shouldBe` 4
    f8_1 3 3 `shouldBe` 61
  it "run f10_1" $ do
    f10_1 6.0 2.7 `shouldBe` (2.0, 0.5999999999999996)
    f10_1 101.0 10.5  `shouldBe` (9.0, 6.5)
  it "run f11_1" $ do
    -- pi/4 == 0.7853981633974483
    f11_1 1 `shouldBe` 0.6666666666666666
    f11_1 100 `shouldBe` 0.7878733502677475
    f11_1 10000 `shouldBe` 0.7854231608976359
  it "run f12_1" $ do
    f12_1 == (1 + 1) `shouldBe` True
    f12_1 == 2 `shouldBe` True
    f12_1 == (1 + 2) `shouldBe` False 