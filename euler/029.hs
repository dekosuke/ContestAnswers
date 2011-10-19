import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

main = print $ Map.size $ Map.fromList [ (a^b,1) | a<-[2..100], b<-[2..100]]
