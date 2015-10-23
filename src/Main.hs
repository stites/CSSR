{-|
 -| Module      : CSSR
 -| Description : The main module of CSSR which imports the three phases of the CSSR algorithm
 -| Maintainer  : sam@stites.io
 -| Stability   : experimental
 -}

module Main where

import Options.Applicative
import CSSR.Parse.Cli
import System.Environment(getArgs)

greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
        ( fullDesc
      <> header "hello - a test for optparse-applicative"
      <> progDesc "Print a greeting for TARGET" )

