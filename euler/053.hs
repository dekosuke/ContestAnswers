import Debug.Trace 

traceS a = trace $ show a

type Z = Integer

fact2 n k = fact2' n k 1
fact2' n k t
  | n==k = t
  | otherwise = fact2' (n-1) k (n*t)

fact n = fact2 n 1

c n r = div (fact2 n (n-r)) (fact r)

main = print $ length [1| n<-[1..100], r<-[1..n], c n r > 1000000]
