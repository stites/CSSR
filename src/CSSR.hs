{-|
 -| Module      : CSSR
 -| Description : The main module of CSSR which imports the three phases of the CSSR algorithm
 -| Maintainer  : sam@stites.io
 -| Stability   : experimental

The Original CSSR is split into three phases which look like the following (note that P^, or S^ is P/S with a circumflex):

CSSR(Alphabet, xbar, L_max, significanceLevel) =
  Phase I- Initialization:
    L <- 0
    States <- {{emptyState}}

  Phase II- Sufficiency:
    while L < Lmax
      TEST(States,
           prob <- P(Xt | X{t-1, t-L} == ax),
           ax | a <- Alphabet, x <- state
           significanceLevel)
      L+1
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

import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  mapM putStrLn args
  -- getFlags args
  return ()

