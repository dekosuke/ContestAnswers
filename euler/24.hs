import Debug.Trace
traceS a b = trace (show a) b

perm :: Ord a => [a] -> [[a]]
perm [] = [[]]
perm xs = foldr (++) [] subs
  where subs = map tempsp [0..length xs - 1]
        tempsp n = map ((xs !! n) :) $ perm (take n xs ++ drop (n+1) xs)

millionth = 999999

main = putStrLn $ foldr (++) "" (map show (perm [0..9] !! millionth))
