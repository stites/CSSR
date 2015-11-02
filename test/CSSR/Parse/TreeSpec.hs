module CSSR.Parse.TreeSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CSSR.Parse.Tree

main :: IO ()
main = hspec $ do
    describe "ParseTree.build" $ do
      it "should transform a simple sequence" $ do
        build [] "abc" `shouldBe` (abcTree :: [ParseTreeBranch])
          where
            abcTree   = [Branch ('a',[Branch ('b',[Branch ('c',[])])])]

      it "should build a tree given multiple calls" $ do
        build (build [] "abc") "aba" `shouldBe` (abcaTree :: [ParseTreeBranch])
        build (build [] "ab" ) "ca"  `shouldBe` (ab_caTree :: [ParseTreeBranch])
          where
            abcaTree  = [Branch ('a',[Branch ('b',[Branch ('c',[]), Branch ('a',[])])])]
            ab_caTree = [Branch ('a',[Branch ('b',[])]),Branch ('c',[Branch ('a',[])])]

--   describe "ParseTree.walk" $ do
--     it "should walk down a simple tree" $ do
--       walk abcTree   1 `shouldBe` ["a"]
--       walk ab_caTree 1 `shouldBe` ["a", "c"]

