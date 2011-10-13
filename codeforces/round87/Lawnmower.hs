import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

traceS a b = trace (show a) b

type Z = Int

data Dir = L | R deriving Show

antiDir L = R
antiDir R = L

solve :: Z -> Z -> [String] -> Z
solve n m ls = solve' n m ls' 0 R
  where ls' = cutter ls

cutter ls = reverse $ cutter' (reverse ls)

cutter' [] = []
cutter' ls = 
  case elemIndex 'W' l of
    Nothing -> cutter' lss
    Just _  -> ls
  where (l:lss) = ls

solve' _ _ [] _ _ = 0
solve' n m [l] c L = c - (elemFind c l)
solve' n m [l] c R = solve' n m [reverse l] (m-c-1) L
solve' n m (l1:lrem@(l2:ls)) c dir = 
  abs (t-c) + solve' n m lrem t (antiDir dir) + 1
  where t = getTarget c m l1 l2 dir

getTarget c m l1 l2 L = min t1 t2
  where t1 = elemFind c l1
        t2 = elemFind c l2
getTarget c m l1 l2 R = m - (1 + getTarget (m-c-1) m (reverse l1) (reverse l2) L)

elemFind c l1 =
  case elemIndex 'W' l1 of
    Nothing -> c
    Just t1 -> min t1 c


main = do
  (n,m) <- (\[a,b] -> (a,b)) . map read . words <$> getLine
  ls <- replicateM n getLine
  print $ solve n m ls
