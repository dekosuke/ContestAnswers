import Debug.Trace

traceS a = trace $ show a

-- took about an hour to solve in Core i7 machine, probably better algorithms exist.

solutions n = solutions' n (n+1) 0
solutions' n x t
 | x > 2 * n = t
 | otherwise = if mod (n*x) (x-n) == 0 then solutions' n (x+1) (t+1) else solutions' n (x+1) t

main = print $ head (filter (\x->solutions x>1000) [1..])
