import Data.List

--take 5min in Core i7 machine

next x 
  | even x = div x 2
  | otherwise = 3 * x + 1 

chain x = chain' x 0
chain' x k  
  | x==1 = (k+1)
  | otherwise = chain' (next x) (k+1)

mils = [1..999999]

print2nd (a,b) = print b

main = print2nd $ (head.reverse) (sort (map (\x->(chain x,x)) mils))
