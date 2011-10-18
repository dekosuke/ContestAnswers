--took 10 secs in Core i7

digits x= map (\x->read [x]) (show x)

fpow x = (sum $ map (\x->x^5) (digits x)) == x

main = print $ sum [x|x<-[2..300000], fpow x]
