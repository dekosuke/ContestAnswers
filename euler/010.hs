import Data.Numbers.Primes --cabal install primes

main = print $ sum (takeWhile (< 2000000) primes)
