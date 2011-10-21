import qualified Data.Map as Map
import Data.Map ((!))
import Debug.Trace 

traceS a = trace $ show a

type Z = Integer

pences = [200,100,50,20,10,5,2,1]

payway n = payway' n pences

payway' :: Z -> [Z] -> Z
payway' 0 _ = 1
payway' _ [] = 0
payway' n (p:ps) = sum $map (\x->payway' (n-p*x) ps)  [0..t]
  where t = div n p

main = print $ payway 200 
