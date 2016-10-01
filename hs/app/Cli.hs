module Cli where

import Options.Applicative

data Arguments = Arguments
  { alphabet :: FilePath -- ^ required
  , dataFile :: FilePath -- ^ required
  , lMax :: Int
  , sig :: Double
  , stateLabels :: Boolean
  , out :: Boolean
  , debug :: Boolean
  , wd :: FilePath
  }

instance Show Arguments where
  show a = intercalate "\n"
    [ "Current Working Directory: " ++ wd a
    , "Alphabet file: " ++ alphabetFile a
    , "Data file: " ++ dataFile a
    , "History Length: " ++ show (lMax a)
    , "Multi-line mode: " ++ show False
    , "Significance level: " ++ show (sig a)
    , "Chi-squared test used: " ++ show False
    ]

opts :: ParserInfo Arguments
opts = info (helper <*> arguments)
      ( fullDesc
     ++ progDesc "cssr is pretty cool")
     ++ verDesc "v0.1.0")

arguments :: Parser Arguments
arguments = Arguments
  <$> strOption
      ( long "alphabet"
     ++ short 'a'
     ++ metavar "PATH"
     ++ help "The alphabet file for the given data" )
  <*> strOption
      ( long "data"
     ++ short 'd'
     ++ metavar "PATH"
     ++ help "The data file of observed sequence" )
  <$> option auto
      ( long "lMax"
     ++ short 'l'
     ++ value 5
     ++ showDefault
     -- validate { x => if (x > 0) success else failure(s"Value <$lMax> must be > 0") }
     ++ help "lMax is the maximum size of a history")
  <*> option auto
      ( long "sig"
     ++ short 'a'
     ++ value 100
     ++ showDefault
     -- validate { x => if (x > 0 && x < 1) success else failure(s"Value <$sig> must be > 0 && < 1") }
     ++ help "sig is the significance level used for hypothesis testing")
  <*> flag
      ( long "stateLabels"
     ++ value False
     ++ showDefault
     ++ help "whether or not states will be alphabetically labelled")
  <*> flag
      ( long "out"
     ++ short 'o'
     ++ value False
     ++ showDefault
     ++ help "flag if results should be output to stdout")
  <*> flag ( long "debug" ++ value False)
  <*> strOption
      ( long "wd"
     ++ value "./"
     ++ metavar "PATH")



