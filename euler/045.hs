import Debug.Trace

traceS a = trace (show a)

-- Take 1min in core i7 machine to execute

tris  = map (\x->x*(x+1) `div` 2) [1..]
pents = map (\x->x*(3*x-1) `div` 2) [1..]
hexs  = map (\x->x*(2*x-1)) [1..]

inters3 t as@(a:ass) bs@(b:bss) cs@(c:css) = 
  --traceS (t,a,b,c) $
  if a==t && b==t && c==t then t : next else next
  where as' = if a==t then ass else as
        bs' = if b==t then bss else bs
        cs' = if c==t then css else cs
        next = inters3 (t+1) as' bs' cs'

findNext :: [Integer] -> Integer
findNext (40755:x:xs) = x
findNext (x:xs) = findNext xs

main = print $ findNext (inters3 1 tris pents hexs)
