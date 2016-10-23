module Data.CSSR.Types where

import CSSR.Prelude

-- | For the moment, an alphabet only consists of symbols of Chars
data Alphabet = Alphabet
  { idxToSym :: Vector Char
  , symToIdx :: HashMap Char Int
  } deriving (Show, Eq)

data HistTree = HL
  { ht_obs :: String
  , frequency :: Vector Integer
  , ht_children :: HashMap Char HistTree
  } deriving (Show)

data LoopingTree = LL
  { obs :: String
  , children :: HashMap Char LoopingTree
  , isLoop :: Bool
  , histories :: HashSet HistTree
  }


