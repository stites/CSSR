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

build :: [ParseTreeBranch] -> [Char] -> [ParseTreeBranch]
build bs@(Branch(bChar, children):[])       (char:[])   = if (char == bChar)
                                                          then bs
                                                          else Branch(char,[]):bs
build bs@(Branch(bChar, children):[])       chars@(char:path) = if (char == bChar)
                                                          then [Branch(bChar, (build children path))]
                                                          else build ( Branch(char,[]):bs ) chars
build bs@(Branch(bChar, children):siblings) (char:[])   = if (char == bChar)
                                                          then bs
                                                          else Branch(char,[]):bs
build bs@(Branch(bChar, children):siblings) chars@(char:path) = if (char == bChar)
                                                          then Branch(bChar, (build children path)):siblings
                                                          else build ( Branch(char,[]):bs ) chars

build [] (char:[]) = build [Branch(char,[])] []
build [] chars@(char:path) = build [Branch(char,[])] chars

build branches [] = branches


