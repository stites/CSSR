{-# LANGUAGE OverloadedStrings #-}
{-|
 -| Module      : CSSR
 -| Description : The main module of CSSR which imports the three phases of the CSSR algorithm
 -| Maintainer  : sam@stites.io
 -| Stability   : experimental

The Original CSSR is split into three phases which look like the following (note that P^, or S^ is P/S with a circumflex):

CSSR(A, xbar, L_max, alpha) =
  Phase I- Initialization:
    L <- 0
    Sigma <- {{unit}}

  Phase II- Sufficiency:
    while L < Lmax
      for each s in Sigma
        estimate P^ (X_t | S^=s)
        for each x in s
          for each a in A
            estimate p <- P^(X_t | X[t-L,t-1] =ax)
            TEST(Sigma, p, ax, s, alpha)
       L <- L+1
    (loopend)

  Phase III- Recursion:
    Remove Transient states from Sigma
    recursive <- False
    until recursive
      recursive <- true
      for each s in Sigma
        for each b in A
          x_o <- first x in s
          T(s,b) <- epsilon^ (x_o * b)
          for each x in s, x /= x_o
            if T(s,b) /= epsilon^ (x * b)
            then create new state s' in Sigma
              T(s',b) <- epsilon^ (x' * b)
              for each y in s such that (epsilon^ (y*b) == epsilon^ (x*b))
                MOVE(y,s,s')
              recursive <- False
TEST(Sigma, p, a*x, s, alpha)
  if null hypothesis (Eq.1) passes a test size of alpha
  then s <- a*x Union s
  else if restricted alterative hypothesis (Eq.2)
          passes a test size alpha for s* in Sigma, s* /= s
  then MOVE(ax,s,s*)
  else create new state s' in Sigma
    MOVE (ax,s,s')

MOVE(x,s1,s2)
  s1 <- s1 \ x
  re-estimate P^(X_t | S^ = s1)
  s2 <- s2 Union x
  re-estimate P^(X_t | S^ = s2)

-}
module CSSR where

import System.Console.GetOpt
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  getFlags args
  return ()

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

