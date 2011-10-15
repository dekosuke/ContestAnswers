nums = map show [1..]
idf = '0' : foldr (\x y->x++y) "" nums

main = print $ foldr (*) 1 (map (\x->read $ [idf !! (10^x)]) [0,1,2,3,4,5,6]::[Int])
