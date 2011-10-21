import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 
import Data.Maybe (fromJust)

--need memorization

traceS a = trace $ show a

type Z = Integer

--2d -> 1d, for memorization, which is one-to-one correspondence
tup2v a b dim = a*dim+b
v2tup n dim = (div n dim, mod n dim)

genMemo :: Z -> Z -> Map.Map Z Z 
genMemo 0 _ = Map.empty
genMemo val dim = Map.insert val ans memo
  where (a,b) = v2tup val dim
        memo = genMemo (val-1) dim
        ans = if a==0 || b==0 then 1 else (fromJust $ Map.lookup (val-1) memo) + (fromJust $ Map.lookup (val-dim) memo)

routes a b = fromJust $ Map.lookup v memo
  where v = tup2v a b 21
        memo = genMemo v 21
{-
routes memo _ 0 = 1
routes memo 0 _ = 1
routes x y = 
  where v1 = unJust $ Map.lookup ((x-1),y) memo 
  routes x (y-1)
-}

main = print $ routes 20 20
