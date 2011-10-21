{-# OPTIONS_GHC -O2 -XBangPatterns #-}

import Data.Numbers.Primes (primes,isPrime)
import Data.List
import Debug.Trace 

--

traceS a = trace $ show a

type Z = Integer

ps = takeWhile (< 1000000) primes

findMaxSumStartFrom x = fmtemp x (sum$drop x mps) (reverse mps)
  where mps = take (x+600) ps
fmtemp !x !tsum !ps@(p:pss) = 
  if tsum<1000000 && isPrime tsum then ((length ps - x),tsum)
  else fmtemp x (tsum-p) pss

--main = print $ (last.sort) consecutivePrimes
main = print $ (snd.last.sort)  
  [findMaxSumStartFrom x | x<-[0..length ps-10]]
