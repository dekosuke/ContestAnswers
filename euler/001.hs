mul35 x = (mod x 3 == 0) || (mod x 5 == 0)

main = print $ sum [x|x<-[1..999], mul35 x]
