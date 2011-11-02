import Debug.Trace 

traceS a = trace $ show a

type Z = Integer

pal x = show x == reverse (show x)

lychrel :: Z -> Bool
lychrel n = lychrel' n 0

lychrel' n k
  | k>50 = True
  | k/=0 && pal n = False
  | otherwise = lychrel' (next n) (k+1)

next n = (read $reverse (show n)) + n

main = print $ length [n | n<-[0..9999], lychrel n]
