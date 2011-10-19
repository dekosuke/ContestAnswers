import Debug.Trace

traceS a = trace (show a)

--took 20min in Core i7 

fact n = fact' n 1
fact' 0 k = k
fact' n k = fact' (n-1) (n*k)

digitFactSum n= sum $ map fact (digits n) 
digits n = map (\x->read [x]) (show n)

main = print $ sum [x| x<-[3..1500000], digitFactSum x == x]
