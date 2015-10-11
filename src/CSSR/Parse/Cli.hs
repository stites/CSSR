{-# LANGUAGE OverloadedStrings #-}
-- | Commandline interface for CSSR
-- THIS IS A WORK IN PROGRESS FILE THAT NEEDS A LOT MORE ATTENTION
module CSSR.Parse.Cli (getFlags) where

import System.Console.GetOpt
import System.Exit

getFlags :: [String] -> IO ()
-- System flags
getFlags[]            = version >> help >> exit
getFlags["-h"]        = version >> help >> exit
getFlags["--help"]    = version >> help >> exit
getFlags["-v"]        = version >> exit
getFlags["--version"] = version >> exit

-- required flags
getFlags["-a"]           = stuff
getFlags["--alphabet"]   = stuff
getFlags["-d"]           = stuff
getFlags["--data"]       = stuff
getFlags["-m"]           = stuff
getFlags["--max-length"] = stuff

-- optional flags
getFlags["-s"]             = stuff
getFlags["--significance"] = stuff
getFlags["-ml"]            = stuff
getFlags["--multiline"]    = stuff
getFlags["-cs"]            = stuff
getFlags["--chi-squared"]  = stuff
-- for everything else
getFlags _ = stuff

help    = putStrLn $ unlines helpstring
version = putStrLn "CSSR, v0.1.0"
exit    = exitWith ExitSuccess
stuff   = version

helpstring = [
    "Usage: cssr [-adlsm] [-cs]",
    "  -h, --help           print this dialouge",
    "  -v, --version        print the program version",
    "\nRequired:",
    "  -a, --alphabet       alphabet file",
    "  -d, --data           data file",
    "  -m, --max-length     maximum string length",
    "\nOptional:",
    "  -s,  --significance  significance level",
    "  -ml, --multiline     parse file and consider multiline",
    "  -cs, --chi-squared   use chi-squared test"
  ]

