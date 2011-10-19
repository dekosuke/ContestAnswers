import Data.Numbers.Primes
import Data.List
import Debug.Trace 

traceS a = trace $ show a

primesSub = takeWhile (< 1000000) $ dropWhile (< 10) primes

truncable x = and $ map isPrime (map read$subs sx)
  where sx = show x

subs :: [a] -> [[a]]
subs xs = xs : (rsubs xs ++ lsubs xs)
lsubs (x:[]) = []
lsubs (x:xs) = xs : lsubs xs
rsubs xs = map reverse (lsubs.reverse$xs)

main = print$sum [x | x<-primesSub, truncable x]
