import Data.Numbers.Primes
import Data.List
import Debug.Trace 

traceS a = trace $ show a

perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = foldr (++) [] subs
  where subs = map tempsp [0..length xs - 1]
        tempsp n = map ((xs !! n) :) $ perm (take n xs ++ drop (n+1) xs)

primesSub = takeWhile (< 10^10) $ dropWhile (< 10) primes

pandigitals = map read$perm "1234567"

main = print$(last.sort) [x | x<-pandigitals, isPrime x]
