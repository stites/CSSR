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

helpstring = [
    "Usage: cssr [-adlsm] [-cs]",
    "  -h, --help           print this dialouge",
    "  -v, --version        print the program version",
    "\nRequired:",
    "  -d, --data           data file",
    "  -m, --max-length     maximum string length",
    "\nOptional:",
    "  -s,  --significance  significance level",
    "  -ml, --multiline     parse file and consider multiline",
    "  -cs, --chi-squared   use chi-squared test"
  ]

