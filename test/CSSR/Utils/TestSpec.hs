module CSSR.Utils.TestSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import CSSR.Utils.Test

main :: IO ()
main = hspec spec

state1 = [ "AAAA",  "AABA",  "ABAA",  "BAAA",  "BABA",
          "AAABA", "AABAA", "ABAAA", "ABABA", "BAABA",
          "BABAA", "BAAAA", "ABAAA", "AAAAA" ]
sigLevel = 0.5

spec :: Spec
spec = do
  describe "CSSR.Utils.Test.nullHypothesis" $ do
    it "should not be significant, given an observation with history ABAB" $ do
      nullHypothesis state1 "BAABAB" > sigLevel `shouldBe` False
      nullHypothesis state1  "AABAB" > sigLevel `shouldBe` False

    it "should be significant, given an observation with history ABAA" $ do
      nullHypothesis state1 "ABABAA" > sigLevel `shouldBe` True
      nullHypothesis state1 "BAABAA" > sigLevel `shouldBe` True

