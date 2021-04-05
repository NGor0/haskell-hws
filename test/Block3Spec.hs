module Block3Spec
 ( spec
 ) where

import Data.List ( nub, sort )
import Test.Hspec ( Spec, describe, it, shouldBe )

import Block3

spec :: Spec
spec = do
  it "toList . fromList == nub . sort" $ do
    toList (fromList [1, 3, 2, 4, 1]) `shouldBe` nub (sort [1, 3, 2, 4, 1])
    toList (fromList $ replicate 5 1) `shouldBe` nub (sort $ replicate 5 1)
  it "run seek" $ do
    seek 2 (fromList [1, 4, 2, 3]) `shouldBe` Just 2
    seek 0 (fromList [1..7]) `shouldBe` Nothing
  it "run add" $ do
    toList (add 0 (fromList [1, 4, 2, 2])) `shouldBe` [0, 1, 2, 4]
    toList (add 6 (fromList [5..7])) `shouldBe` [5..7]
  it "run kmin" $ do
    kmin 3 (fromList [1, 4, 5, 2, 2]) `shouldBe` [1, 2, 4]
    kmin 10 (fromList $ replicate 10 1) `shouldBe` [1]
  it "run TST fmap" $ do
    toList (fmap succ (fromList [1..4])) `shouldBe` [2..5]
    toList (fmap succ (fromList ['a', 'c', 'a'])) `shouldBe` ['b', 'd']
  it "run natSum" $ do
    natSum Z (S (S Z)) `shouldBe` S (S Z)
    natSum (S (S Z)) (S (S (S Z))) `shouldBe` S (S (S (S (S Z))))
  it "run natMult" $ do
    natMult Z Z `shouldBe` Z
    natMult (S Z) (S (S Z)) `shouldBe` S (S Z)
    natMult (S (S Z)) (S (S (S Z))) `shouldBe` S (S (S (S (S (S Z)))))
  it "run superNat" $ do
    superNat Z (S (S (S Z))) (S (S Z)) `shouldBe` S (S (S (S (S Z))))
    superNat (S Z) (S (S (S Z))) (S (S Z)) `shouldBe` S (S (S (S (S (S Z)))))
    superNat (S (S Z)) (S (S (S Z))) (S (S Z)) `shouldBe` S (S (S (S (S (S (S (S (S Z))))))))
  it "run Ord N" $ do
    Z < S Z `shouldBe` True
    S (S Z) == Z `shouldBe` False
    S (S Z) >= S Z `shouldBe` True
