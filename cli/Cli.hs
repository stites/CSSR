-- | Commandline interface for CSSR
module Cli where

import Options.Applicative
import Options.Applicative.Builder

{-
data Arguments = Arguments {
  version      :: Bool,
  alphabet     :: String,
  dataUri      :: String,
  maxLength    :: Int,
  significance :: String,
  multiline    :: String,
  chiSquared   :: String
  }

cli :: Parser Arguments
cli = Arguments
     -- Meta:
     <$> versionFlag

     -- Required:
     <*> alphabetOption
     <*> dataOption
     <*> maxLengthOption

     -- Optional:
     <*> significanceOption
     <*> multilineFlag
     <*> chiSquaredFlag

-- ==============================
-- Program Flags
-- ------------------------------
-- TODO: update this
versionFlag :: Parser Bool
versionFlag = switch
    ( long "version"
   <> short 'v'
   <> help "Show the version" )

-- ==============================
-- Required Options
-- ------------------------------
-- these are options which are mandatory to run CSSR. All required options have
-- both short and long forms A regular option in CSSR has both a long and short
-- name, and can be specified on the commandline as one of the following (here
-- "alphabet" or "a" is used as an example):
--
--     --alphabet filename.txt
--     --alphabet=filename.txt
--     --a filename.txt
--     --afilename.txt
-- ------------------------------

alphabetOption :: Parser String
alphabetOption = strOption
   ( long "alphabet"
  <> short 'a'
  <> metavar "FILE"
  <> help "Required. A file containing the alphabet of all possible symbols." )

dataOption :: Parser String
dataOption = strOption
   ( long "data"
  <> short 'd'
  <> metavar "FILE"
  <> help "Required. A file containing the data to be modelled." )

maxLengthOption :: Parser Int
maxLengthOption  = option auto
   ( long "max-length"
  <> short 'l'
  <> metavar "N"
  <> help "Required. The maximum size of events that a state can contain." )

-- ==============================
-- Optional Flags and Options
-- ------------------------------
-- TODO: ensure that this has respectable defaults
significanceOption :: Parser Integer -- should be a float
significanceOption = option auto
    ( long "significance"
   <> short 's'
   <> metavar "S"
   <> help "Set the significance level (defaults to 0.5)." )

multilineFlag :: Parser Bool
multilineFlag = switch
    ( long "multi-line"
   <> short 'm'
   <> help "Indicate that multiple lines should be considered when parsing the data file." )

chiSquaredFlag :: Parser Bool
chiSquaredFlag = switch
    ( long "chi-squared"
   <> short 'c'
   <> help "Use the Chi-Squared test." )

-}
