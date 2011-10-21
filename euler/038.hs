import Data.List (sort)

type Z = Integer

main = print $ (last.sort) [pand n|n<-[9999,9998..100]]

pand :: Z -> Z
pand x = maximum $ map (\k->pandk k x) [2..5]

pandk :: Z -> Z -> Z
pandk k x 
  | sort ctt == "123456789" = read ctt
  | otherwise = 0
  where ctt = ct x k 

--really dumb algorithm
ct :: Z -> Z -> String
ct x 1 = show x
ct x k = ct x (k-1) ++ show (k*x)
