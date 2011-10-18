import Data.List

type Z = Integer

factor n = factor' n 2
factor' :: Z -> Z -> [Z]
factor' n k
  | n < k*k = [n]
  | mod n k == 0 = k : factor' (div n k) 2
  | otherwise = factor' n (k+1)

main = print $ head (reverse (sort $ factor 600851475143))
