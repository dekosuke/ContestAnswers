import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

type Z = Int

-- based on Rotsor's solution

traceS a b = trace (show a) b

mLength :: Map.Map Z Z -> Z -> Z
mLength hs (-1) = 0
mLength hs i = mLength hs (hs!i) + 1

main = do
  n <- read <$> getLine
  hs <- replicateM n (read <$> getLine)
  let m = Map.fromList (zip [1..] hs)
  print $ maximum (map (mLength m) [1..n])
