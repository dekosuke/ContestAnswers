import Data.Numbers.Primes (primes,isPrime)

rotations xs = map (rotate xs) [1..length xs]

rotate xs k = (drop k xs) ++ (take k xs)

isCircularPrime a = and $ map (isPrime.read) (rotations $ show a)

main = print.length $ filter isCircularPrime (takeWhile (< 1000000) primes)
