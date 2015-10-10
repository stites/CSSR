-- Parse Tree
module CSSR.ParseTree (parseTree) where

import Debug.Trace

data ParseTree = Root [ParseTreeBranch] deriving Show

data ParseTreeBranch = Branch (Char, [ParseTreeBranch]) deriving Show

parseTree :: ParseTree
parseTree = Root []

exampleBranchArray = [
  Branch ('a', [
    Branch ('b',[
      Branch ('c',[]),
      Branch ('a',[])
    ]),
    Branch ('c',[
      Branch ('c',[]),
      Branch ('a',[])
    ])
  ]) ]

exampleParseTree = Root exampleBranchArray

-- | buildParseTree takes a list of characters and generates a ParseTree
buildParseTree :: [ParseTreeBranch] -> [Char] -> [ParseTreeBranch]
-- | if we have a sparse tree and a char-sequence
buildParseTree branches@(Branch(bChar, children):[])
                  chars@(char:path)                  = if (char == bChar)
                                                       then if (null path)
                                                            then branches
                                                            else [Branch(bChar, (buildParseTree children path))]
                                                       else if (null path)
                                                            then Branch(char, []):branches
                                                            else buildParseTree ( Branch(char,[]):branches ) chars
-- | if we have a full tree and a char-sequence
buildParseTree branches@(Branch(bChar, children):siblings)
                  chars@(char:path)                  = if (char == bChar)
                                                       then if (null path)
                                                            then branches
                                                            else Branch(bChar, (buildParseTree children path)):siblings
                                                       else if (null path)
                                                            then Branch(char,[]):branches
                                                             else buildParseTree ( Branch(char,[]):branches ) chars
-- | if we have an empty tree
buildParseTree []       (char:[])   = buildParseTree [Branch(char,[])] []
buildParseTree [] chars@(char:path) = buildParseTree [Branch(char,[])] chars

