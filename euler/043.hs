import Data.List
import Debug.Trace 

traceS a = trace $ show a

perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = foldr (++) [] subs
  where subs = map tempsp [0..length xs - 1]
        tempsp n = map ((xs !! n) :) $ perm (take n xs ++ drop (n+1) xs)

pandigitals = perm "0123456789"

td k = (take 3) . (drop k)

ps = [2,3,5,7,11,13,17]
divProperty x = and $ map (\k->mod (read.(td k)$x) (ps!!(k-1)) == 0) [1..7]

main = print $ sum [read x| x<-pandigitals, divProperty x]
