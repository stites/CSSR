{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative (execParser)

import Cli

main :: IO ()
main = execParser opts >>= runCssr

runCssr :: Arguments -> IO ()
runCssr Arguments{..} =
  if out
  then undefined
  else undefined


