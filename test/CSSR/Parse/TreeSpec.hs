module CSSR.Parse.TreeSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CSSR.Parse.Tree

abcTree, abcaTree, abcaaTree, ab_caTree :: [ParseTreeBranch]

abcTree   = [Branch ('a',[Branch ('b',[Branch ('c',[])])])]
abcaTree  = [Branch ('a',[Branch ('b',[Branch ('a',[]), Branch ('c',[])])])]
ab_caTree = [Branch ('c',[Branch ('a',[])]),Branch ('a',[Branch ('b',[])])]
abcaaTree = [Branch ('a',[Branch ('b',[Branch ('a',[Branch ('a',[])]),Branch ('c',[])])])]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ParseTree.build" $ do
    it "should transform a simple sequence" $ do
      (build [] "abc") `shouldBe` abcTree

    -- TODO: make building a parseTree monadic
    -- TODO: define Eq on parseTree so that it interprets sets
    it "should build a tree given more than one call" $ do
      build (build [] "abc") "aba"                `shouldBe` abcaTree
      build (build [] "ab" ) "ca"                 `shouldBe` ab_caTree
      build (build (build [] "abc") "aba") "abaa" `shouldBe` abcaaTree

  describe "ParseTree.walk" $ do
    it "should walk down a simple tree" $ do
      pending
--        walk abcTree   1 `shouldBe` ["a"]
--        walk ab_caTree 1 `shouldBe` ["a", "c"]

