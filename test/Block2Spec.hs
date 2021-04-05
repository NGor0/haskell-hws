module Block2Spec
 ( spec
 ) where

import Test.Hspec ( Spec, describe, it, shouldBe, shouldNotBe )

import Block2

spec :: Spec
spec = do
  it "run antisort" $ do
    antisort [1..3] `shouldNotBe` [1..3]
    antisort [3,2..1] `shouldNotBe` [3,2..1]
  it "run antiprimes" $ do
    antiprimes 1 `shouldBe` [1]
    antiprimes 5 `shouldBe` [1, 4, 6, 8, 9]
  it "run antiunion" $ do
    antiunion [1..3] [1..3] `shouldBe` []
    antiunion [1, 2, 3] [2, 3, 4] `shouldBe` [1, 4]
  it "run antimerge" $ do
    antimerge [1, 2, 1, 2, 1] `shouldBe` [(3, 1), (2, 2)]
    antimerge (replicate 5 1) `shouldBe` [(5, 1)]
  it "run antiintercalate" $ do
    antiintercalate [1..3] `shouldBe` [(1, 1), (1, 2), (1, 3)]
    antiintercalate [1, 1, 1, 2, 2, 1, 1, 1, 3]
        `shouldBe` [(3, 1), (2, 2), (3, 1), (1, 3)]
  it "run antiantiintercalate" $ do
    antiantiintercalate [(10, 1)] `shouldBe` replicate 10 1
    antiantiintercalate [(3, 1), (2, 2), (3, 1), (1, 3)]
      `shouldBe` [1, 1, 1, 2, 2, 1, 1, 1, 3]
  it "run getNumberOrNot" $ do
    getNumberOrNot "1bc" `shouldBe` Nothing
    getNumberOrNot " 123" `shouldBe` Just 123
    getNumberOrNot "-123" `shouldBe` Just (-123)
    getNumberOrNot "123   456" `shouldBe` Just 123456
  it "run getMailDomainOrNot" $ do
    getMailDomainOrNot "bad-adress@mail.r" `shouldBe` Nothing
    getMailDomainOrNot "super-bad-adress-lol@gmail.com"
      `shouldBe` Just "gmail.com"
  it "run maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot" $ do
    maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot
      (Just (Just (Just (Just (Just (Just 10)))))) 0 `shouldBe` 10
    maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot
      (Just (Just (Just (Just Nothing )))) '0' `shouldBe` '0'
    maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot
      Nothing "" `shouldBe` ""
  it "run stupidTraverse" $ do
    stupidTraverse [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 7]
      `shouldBe` Just [(1, 2, 3, 4), (5, 6, 7, 7)]
    stupidTraverse [Nothing, Nothing, Just 3, Nothing]
      `shouldBe` Nothing
