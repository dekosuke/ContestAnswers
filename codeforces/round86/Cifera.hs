import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

traceS a b = trace (show a) b

type Z = Integer

mz :: Z
mz = 2^32

solve :: Z -> Z -> Z -> String
solve k c n
  | t > mz = "NO"
  | t == c = "YES\n" ++ (show (n-1))
  | otherwise = solve k c (n+1)
  where t = k^n

main = do
  k <- read  <$> getLine
  c <- read <$> getLine
  putStrLn $ solve k c 1
