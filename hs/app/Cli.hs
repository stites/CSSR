module Cli where

import Options.Applicative
import Data.List

data Arguments = Arguments
  { alphabet :: FilePath -- ^ required
  , dataFile :: FilePath -- ^ required
  , lMax :: Int
  , sig :: Double
  , stateLabels :: Bool
  , out :: Bool
  , debug :: Bool
  , wd :: FilePath
  }

instance Show Arguments where
  show a = intercalate "\n"
    [ "Current Working Directory: " ++ wd a
    , "Alphabet file: " ++ alphabet a
    , "Data file: " ++ dataFile a
    , "History Length: " ++ show (lMax a)
    , "Multi-line mode: " ++ show False
    , "Significance level: " ++ show (sig a)
    , "Chi-squared test used: " ++ show False
    ]

opts :: ParserInfo Arguments
opts = info (helper <*> arguments)
     (fullDesc
     <> progDesc "cssr is a program to blindly construct HMMs"
     <> header "v0.1.0")

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption
      ( long "alphabet"
     <> short 'a'
     <> metavar "PATH"
     <> help "The alphabet file for the given data" )
  <*> strOption
      ( long "data"
     <> short 'd'
     <> metavar "PATH"
     <> help "The data file of observed sequence" )
  <*> option auto
      ( long "lMax"
     <> short 'l'
     <> value 5
     <> showDefault
     -- validate { x => if (x > 0) success else failure(s"Value <$lMax> must be > 0") }
     <> help "lMax is the maximum size of a history")
  <*> option auto
      ( long "sig"
     <> short 'a'
     <> value 100
     <> showDefault
     -- validate { x => if (x > 0 && x < 1) success else failure(s"Value <$sig> must be > 0 && < 1") }
     <> help "sig is the significance level used for hypothesis testing")
  <*> flag True False
      ( long "labels"
     <> showDefault
     <> help "whether or not states will be alphabetically labelled")
  <*> flag False True
      ( long "out"
     <> short 'o'
     <> showDefault
     <> help "flag if results should be output to stdout")
  <*> flag False True
      ( long "debug" )
  <*> strOption
      ( long "wd"
     <> value "./"
     <> metavar "PATH")



