module CSSR.Parse.Tree (
  ParseTree(Root),
  ParseTreeBranch(Branch),
  build,
  walk,
  getBranches
  ) where

import CSSR.CausalState.State (Event, State)

data ParseTree = Root [ParseTreeBranch] deriving Show
data ParseTreeBranch = Branch (Event, [ParseTreeBranch]) deriving Show

-- | build takes a list of characters and generates a ParseTree
build :: [ParseTreeBranch] -> [Event] -> [ParseTreeBranch]
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

walk :: [ParseTreeBranch] -> Int -> State
walk tree@(    Branch(m,          []):[]    ) depth | depth >= 0 = [[m]]
walk tree@(    Branch(m, children:[]):[]    ) depth | depth >= 0 = map ((:) m) (walk [children] $ depth-1)
-- MISSING: a branch having many children and no siblings
walk tree@(    Branch(m,        []):siblings) depth | depth >= 0 = [m]:(walk siblings depth)
-- MISSING: a branch having one child and many siblings
walk tree@( b@(Branch(m, children)):siblings) depth | depth >= 0 = (walk [b] depth) ++ (walk siblings depth)
walk _ _ = []

getBranches :: ParseTree -> [ParseTreeBranch]
getBranches tree@(Root branches) = branches
