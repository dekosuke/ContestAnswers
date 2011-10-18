import Data.List

type Z = Integer


{-
factor n = factor' n 2
factor' :: Z -> Z -> [Z]
factor' n k
  | n < k*k = [n]
  | mod n k == 0 = k : factor' (div n k) 2
  | otherwise = factor' n (k+1)
-}

sumd a b = sum (map (\x->read [x]) $ show (a ^ b))

main = print $ head (reverse (sort [sumd a b| a<-[1..100], b<- [1..100]]))
