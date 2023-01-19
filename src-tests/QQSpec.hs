-- | This module tests the mala quasi-quoter interface
{-# LANGUAGE QuasiQuotes #-}
module QQSpec where

import DepTutMain
import DepTutQQ

import Test.Hspec

spec :: Spec
spec = do
  describe "The Mala QuasiQuoter" $ do
    it "parses a plain lambda identity function" $ do
      [mala|λx . x |] `shouldBe` (lam id 0)
    it "parses *" $ do
      [mala|*|] `shouldBe` (TermInferred TermStar)
    it "parses applications" $ do
      [mala|λf.λx.f x|] `shouldBe` (lam (\f -> lam (\x -> f `app` x)) 0)
      [mala|λf.λx.λy.f x y|] `shouldBe` (lam (\f -> lam (\x -> lam (\y -> (f `app` x) `app` y))) 0)
        {-
    it "parses parenthesised expressions" $ do
      [mala|λx . (x) |] `shouldBe` (lam id 0)
      -- [mala|(λx . (x) )|] `shouldBe` (lam id 0)
      [mala|λx.x x|] `shouldBe` (lam id 0)
-}
