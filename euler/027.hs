import Data.Numbers.Primes (isPrime)
import Data.List (sort)

contPrimes a b = contPrimes' a b 0
contPrimes' a b n 
  | isPrime (n*n+a*n+b) == True = contPrimes' a b (n+1)
  | otherwise = n

main = print $ (snd.last.sort) [(contPrimes a b,a*b)|a<-[-999..999],b<-[-999..999]]
