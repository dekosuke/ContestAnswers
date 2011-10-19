pal10 x = x' == reverse x'
  where x' = show x

pal2 x = x' == reverse x'
  where x' = binshow x

--not efficient..
binshow = reverse.binshow'
binshow' x 
  | x == 0 = []
  | mod x 2 == 1 = '1' : binshow' (div x 2)
  | otherwise = '0' : binshow' (div x 2)

main = print $ sum [x| x<-[1..1000000], pal10 x , pal2 x]
