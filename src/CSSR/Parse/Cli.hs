-- | Commandline interface for CSSR
module CSSR.Parse.Cli where

import Options.Applicative
import Options.Applicative.Builder

data Sample = Sample { alphabet :: String , version :: Bool }

sample :: Parser Sample
sample = Sample
     <$> alphabetFile
     <*> getVersion

getVersion = switch
    ( long "version"
   <> short 'v'
   <> help "Show the version" )

-- ==============================
-- Regular Options
-- ------------------------------
-- these are considered mandatory and have both short and long forms A regular
-- option in CSSR has both a long and short name, and can be specified on the
-- commandline as one of the following (here "alphabet" or "a" is used as an
-- example):
--     --alphabet filename.txt
--     --alphabet=filename.txt
--     --a filename.txt
--     --afilename.txt

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

helpstring = [
    "Usage: cssr [-adlsm] [-cs]",
    "  -v, --version        print the program version",
    "\nOptional:",
    "  -s,  --significance  significance level",
    "  -ml, --multiline     parse file and consider multiline",
    "  -cs, --chi-squared   use chi-squared test"
  ]

