import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace

type Z = Int

myRead :: IO String
myRead = getLine

pos :: Z -> Z -> [String] -> Char
pos x y ls = (ls !! y) !! x

adjs x y m n ls =
  (al x y m n ls) ++ (ar x y m n ls) ++ (au x y m n ls) ++ (ad x y m n ls)

al x y m n ls = if x>0 then [pos (x-1) y ls] else []
ar x y m n ls = if (+) x 1 < m then [pos (x+1) y ls] else []
au x y m n ls = if y>0 then [pos x (y-1) ls] else []
ad x y m n ls = if (+) y 1 < n then [pos x (y+1) ls] else []

captWolf :: Z -> Z -> Z -> Z -> [String] -> Z
captWolf x y m n ls 
  | pos x y ls /= 'W'  = 0
  | otherwise = if elem 'P' $ adjs x y m n ls then 1 else 0

solve :: Z -> Z -> [String] -> Z
solve m n ls = sum [captWolf x y m n ls | x <- [0..(m-1)], y <- [0..(n-1)] ]

main = do
  (n,m) <- (\[a,b] -> (a,b)) . map read . words <$> getLine
  ls <- replicateM n myRead
  print $ solve m n ls