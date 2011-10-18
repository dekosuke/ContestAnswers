import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

type Z = Integer

factor :: Z -> Map.Map Z Z
factor n = factor' n 2
factor' n k
  | n < k*k = Map.fromList [(n,1)]
  | mod n k == 0 = Map.insertWith (+) k 1 (factor' (div n k) 2)
  | otherwise = factor' n (k+1)

tris :: [Z]
tris = tris' 0 [1..]

tris' :: Z -> [Z] -> [Z]
tris' x (n:ns) = (x+n) : tris' (x+n) ns

divisorsNum :: Z -> Z
divisorsNum n = foldr (\(k,n) x->x*(n+1)) 1 $ Map.toList fs
  where fs = factor n

main = print $ head (dropWhile (\x->divisorsNum x <= 500) tris)
