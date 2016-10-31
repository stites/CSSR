{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Parse.Tree where

import GHC.TypeLits
import Data.Proxy
import Control.Monad.ST
import Data.STRef
import Control.Exception (assert)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Hashable
import Lens.Micro.Internal

import CSSR.Prelude
import CSSR.TypeAliases


data ParseTree = ParseTree
  { depth :: Int
  , root :: PLeaf
  } deriving (Show, Eq)

data PLeaf = PLeaf
  { _body :: PLeafBody
  , _children :: Children
  } deriving (Show, Eq)

data PLeafBody = PLeafBody
  { _obs       :: Vector Event
  , _count     :: Integer
  , _locations :: Locations
  } deriving (Show, Eq)

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (=10)
-- We then take the 1 child of 10 (=110)
--------------------------------------------------------------------------------


type instance Index PLeaf = Vector Event
type instance IxValue PLeaf = PLeaf

-- set (ix (V.fromList "9") . body . count)  50000 mkRoot
--
instance Ixed PLeaf where
  ix :: Vector Event -> Traversal' PLeaf (IxValue PLeaf)
  ix histories = go 0
    where
      go depth f p@(PLeaf body childs)
        | V.length histories == depth = f p
        | otherwise =
          case HM.lookup c childs of
            Nothing -> pure p
            Just child -> goAgain <$> go (depth+1) f child
        where
          c :: Event
          c = histories V.! depth

          goAgain :: PLeaf -> PLeaf
          goAgain child' = PLeaf body (HM.insert c child' childs)

navigate :: ParseTree -> Vector Event -> Maybe PLeaf
navigate tree history = (root tree) ^? ix history


-- mkRoot & over (path (fromList "abc") . count) (+1)
--
path :: forall f. Applicative f
             => Vector Event
             -> (PLeafBody -> f PLeafBody)
             -> PLeaf
             -> f PLeaf
path events fn = go 0
  where
    go :: Int -> PLeaf -> f PLeaf
    go depth (PLeaf body childs) =
      assert (V.take depth events ==  _obs body) $
        if depth == V.length events - 1
        then PLeaf <$> fn body <*> pure childs
        else PLeaf <$> fn body <*> nextChilds

      where
        nextChilds :: f (HashMap Event PLeaf)
        nextChilds =
          case HM.lookup c childs of
            Just child -> HM.insert c <$> go (depth + 1) child <*> pure childs
            Nothing -> HM.insert c <$> buildNew depth <*> pure childs
          where
            c :: Event
            c = V.unsafeIndex events depth


        buildNew :: Int -> f PLeaf
        buildNew depth
          | depth == V.length events - 1 = PLeaf <$> mkBod events <*> pure mempty
          | otherwise = PLeaf <$> mkBod es <*> childs
          where
            c :: Event
            c = V.unsafeIndex events (depth + 1)

            es :: Vector Event
            es = V.take (depth + 1) events

            mkBod :: Vector Event -> f PLeafBody
            mkBod es' = fn (PLeafBody es' 0 mempty)

            childs :: f (HashMap Char PLeaf)
            childs = HM.singleton c <$> buildNew (depth + 1)


type Event = Char
type Children = HashMap Event PLeaf
type Parent = Maybe PLeaf
type DataFileContents = Vector Event


-- data LoopingTree = LoopingTree
--   { root :: LLeaf
--   , terminals:: Set LLeaf
--   , edges :: Set LLeaf
--   } deriving (Show, Eq)
--
-- data LLeaf = LLeaf
--   { obs       :: Vector Event
--   , histories :: Set PLeaf
--   , children  :: HashMap Char LLeaf
--   , distribution :: Vector Float
--   , isTerminal :: Bool
--   , isEdge :: Bool
--   , isLoop :: Bool
--   }

-- make a note: steal mitchell's brain
-- study all of the monoids
-- do in-depth dives for each of the lens modules

-- type Alphabet = [Event]
--
makeLenses ''PLeafBody
makeLenses ''PLeaf

current :: Vector Event -> Event
current = V.last

prior :: Vector Event -> Vector Event
prior = V.init

mkRoot :: PLeaf
mkRoot = PLeaf (PLeafBody [] 0 mempty) mempty

mkLeaf :: Vector Event -> PLeaf
mkLeaf obs = PLeaf (PLeafBody obs 0 mempty) mempty

buildTree :: Int -> DataFileContents -> ParseTree
buildTree n' chars = ParseTree n' root
  where
    n :: Int
    n = n' + 1

    root :: PLeaf
    root = V.ifoldr reducer mkRoot chars

    reducer :: Int -> Event -> PLeaf -> PLeaf
    reducer i _ tree = tree & over (path (sliceEvents i) . count) (+1)

    sliceEvents :: Int -> Vector Event
    sliceEvents i
      | i + n < length chars = V.slice i n . V.filter isValid $ chars
      | otherwise = V.slice i (length chars - i) . V.filter isValid $ chars

    isValid :: Event -> Bool
    isValid e = not $ HS.member e ['\r', '\n']


