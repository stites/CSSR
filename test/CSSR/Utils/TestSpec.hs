module CSSR.Utils.TestSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import CSSR.Utils.Test

main :: IO ()
main = hspec spec

state1 = [ "AABA", "ABAA", "BAAA", "BABA", "AAABA", "AABAA", "ABAAA", "ABABA", "BAABA", "BABAA" ]
sigLevel = 0.5

spec :: Spec
spec = do
  describe "CSSR.Utils.Test.nullHypothesis" $ do
    it "should not be significant, given observed ABAB and the test state" $ do
      nullHypothesis state1 "ABAB" > sigLevel `shouldBe` False

