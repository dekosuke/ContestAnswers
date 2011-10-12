import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace

type Z = Integer

myRead :: IO (Z,Z)
myRead = (\[a,b] -> (a,b)) . map read . words <$> getLine

iDiff :: (Z,Z) -> Z
iDiff (a,b) = b-a

traceS x y = trace (show x) y

--solve :: [(Z,Z)] -> Z
solve abs = maximum rs 
  where rs = solve' 0 abs

solve' :: Z -> [(Z,Z)] -> [Z]
solve' _ [] = []
solve' x (ab:abs) = x' : solve' x' abs 
  where x' = x + iDiff ab

main = do
  n <- read <$> getLine
  abs <- replicateM n myRead
  print $ solve abs