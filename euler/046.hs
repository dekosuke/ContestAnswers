import Debug.Trace 
import Data.Numbers.Primes (primes, isPrime)

traceS a = trace $ show a

type Z = Integer

twiceSqs = [2*x*x|x<-[1..]]

composites = [x|x<-[4..], isPrime x == False]

oddComposites :: [Z]
oddComposites = filter (\x->mod x 2==1) composites

isTwiceSq :: Z->Bool
isTwiceSq x
  | mod x 2 == 1 = False
  | isSq x' = True
  | otherwise = False
  where x' = div x 2 

isSq x = isSq' x 1
isSq' x k 
 | t > x = False
 | t == x = True
 | otherwise = isSq' x (k+1)
 where t = k*k

nonGoldbach :: Z -> Bool
nonGoldbach x = length (filter isTwiceSq $ map (\y->x-y) (takeWhile (< x) primes)) == 0

--main = print $ isTwiceSq 32
main = print $ head (filter nonGoldbach oddComposites)
