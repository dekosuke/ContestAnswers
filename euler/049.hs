import Data.Numbers.Primes
import Data.List

primes4digit = takeWhile (< 10000) $ dropWhile (< 1000) primes

main = print [(x,x+y,x+2*y)| x<-primes4digit, y<-[1..4999], x+2*y<10000, isPrime$x+y, isPrime$x+2*y, (sort.show$x)==(sort.show$x+y), (sort.show$x)==(sort.show$x+2*y), x/=1487]
