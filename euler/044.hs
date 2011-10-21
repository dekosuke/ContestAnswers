import qualified Data.Map as Map
import Data.Map ((!))
import Data.List
import Debug.Trace 

traceS a = trace $ show a

pents = [div (n*(3*n-1)) 2 | n<-[1..]]
pentsmap = Map.fromList $ zip pents [1..20000]

pdiff = pdiff' pents
pdiff' (p1:ps@(p2:pss)) = (p2-p1) : pdiff' ps

pentsSub = take 10000 pents

pexist a = Map.member a pentsmap

main = print $ (head.sort) 
 [ pk-pj | pk<-pentsSub, pj<-pentsSub, pexist$pk-pj, pexist$pk+pj]
