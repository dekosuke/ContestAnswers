type Z=Int

main = print.sum $ take ((1001+1)*2-3) ds
ds=1:f [3,5..] 0 0

f :: [Z] -> Z -> Z -> [Z]
f (x:xs) 3 l = x : f (drop (l+1) xs) 0 (l+1)
f (x:xs) k l = x : f (drop l xs) (k+1) l
