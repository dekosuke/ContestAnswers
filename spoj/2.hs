{-# OPTIONS_GHC -O2 -XBangPatterns #-}

--works fine, but TLE...

import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

traceS a b = trace (show a) b

type Z = Integer

myRead = (\[a,b] -> (a,b)) . map (read::String->Z) . words <$> getLine

primes  = 4 : primes' where
  primes' = 3 : sieve 0 5
  sieve i x = filter isPrime [x,x+2..p*p-2] ++ sieve (i+1) (p*p+2) where
    (ps,p:_) = splitAt i primes'
    isPrime x = all ((/=0).rem x) ps
myPrimes = take 4000 primes 

solve (n,m) = unlines.map show  $ solve' [max n 2..m] myPrimes

solve' :: [Z] -> [Z] -> [Z]
solve' ns [] = ns
solve' ns (p:ps)
  = solve' (filter (\x->(x == p) || ((x `mod` p) /=0))  ns) ps

main = do
  l <- read <$> getLine
  nms <- replicateM l myRead
  mapM_ (putStrLn.solve) nms
