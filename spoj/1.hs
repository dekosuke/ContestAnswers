import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

traceS a b = trace (show a) b

type Z = Int

main = do
  ls <- words <$> getContents
  mapM_ putStrLn $ fst (break (== "42") ls)
