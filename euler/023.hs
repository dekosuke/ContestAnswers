import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

--took 1 min in Core i7

type Z = Integer

factor :: Z -> Map.Map Z Z
factor n = factor' n 2
factor' n k
  | n == 1 = Map.empty
  | n < k*k = Map.fromList [(n,1)]
  | mod n k == 0 = Map.insertWith (+) k 1 (factor' (div n k) 2)
  | otherwise = factor' n (k+1)

dsum :: Z -> Z
dsum n = dsum' n - n
dsum' n = foldr (\(k,n) x->x*facp k n) 1 $ Map.toList fs
  where fs = factor n
        facp k n = div (k^(n+1) - 1) (k-1)

abundant x = dsum x > x

abundants :: [Z]
abundants = filter abundant [1..30000]

abundantPairSums :: Map.Map Z Bool
abundantPairSums = Map.fromList $ zip [x+y|x<-abundants, y<-abundants, x<=y] (cycle [True])

--main = print $ dsum 220
main = print $ sum [x|x<-[1..30000], Map.notMember x abundantPairSums]
