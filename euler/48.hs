tenbillion = 10000000000

tend n t 0 = n
tend n t k = tend (mod (n*t) tenbillion) t (k-1)

main = print $ mod s tenbillion
  where s = sum $ map (\x->tend 1 x x) [1..1000]
