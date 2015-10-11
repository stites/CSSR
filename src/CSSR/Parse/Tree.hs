module CSSR.Parse.Tree where

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

-- | build takes a list of characters and generates a ParseTree
build :: [ParseTreeBranch] -> [Char] -> [ParseTreeBranch]
-- | if we have a sparse tree and a char-sequence
build branches@(Branch(bChar, children):[])
                  chars@(char:path)                  = if (char == bChar)
                                                       then if (null path)
                                                            then branches
                                                            else [Branch(bChar, (build children path))]
                                                       else if (null path)
                                                            then Branch(char, []):branches
                                                            else build ( Branch(char,[]):branches ) chars
-- | if we have a full tree and a char-sequence
build branches@(Branch(bChar, children):siblings)
                  chars@(char:path)                  = if (char == bChar)
                                                       then if (null path)
                                                            then branches
                                                            else Branch(bChar, (build children path)):siblings
                                                       else if (null path)
                                                            then Branch(char,[]):branches
                                                             else build ( Branch(char,[]):branches ) chars
-- | if we have an empty tree
build []       (char:[])   = build [Branch(char,[])] []
build [] chars@(char:path) = build [Branch(char,[])] chars

build branches _ = branches

-- | fold a collection of lists into a parsetree

