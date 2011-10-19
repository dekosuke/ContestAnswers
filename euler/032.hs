import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

ns="123456789"

perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = foldr (++) [] subs
  where subs = map tempsp [0..length xs - 1]
        tempsp n = map ((xs !! n) :) $ perm (take n xs ++ drop (n+1) xs)

splits :: [(Int,Int)]
splits = [(x,y)|x<-[1..7], y<-[1..(8-x)]]

split3 :: [a] ->  (Int,Int) -> ([a], [a], [a])
split3 xs (a,b) = (as,bs,cs)
  where (as,ts) = splitAt a xs
        (bs,cs) = splitAt b ts

ident :: (String, String, String) -> Bool
ident (a,b,c) = (read a) * (read b) == (read c)

getProduct :: (String, String, String) -> Int
getProduct (a,b,c) = read c

getProductFold xs = (Map.foldrWithKey (\k x y->k+y) 0).Map.fromList $ zip (map getProduct xs) (cycle [1])

main = print $ getProductFold (filter ident [split3 xs s| xs <- perm ns, s<-splits])
--wrong answer..
