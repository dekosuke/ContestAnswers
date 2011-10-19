import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

type Z = Integer

factor :: Z -> Map.Map Z Z
factor n = factor' n 2
factor' n k
  | n == 1 = Map.empty
  | n < k*k = Map.fromList [(n,1)]
  | mod n k == 0 = Map.insertWith (+) k 1 (factor' (div n k) 2)
  | otherwise = factor' n (k+1)

fourFactors n = Map.size (factor n) == 4

findContinuousFour xs = fcf' xs 0
fcf' (x:xs) 3 = x-3
fcf' (x:xs@(x2:xss)) k
  | x2-x == 1 = fcf' xs (k+1)
  | otherwise = fcf' xs 0

main=print $ findContinuousFour (filter fourFactors [2..])

