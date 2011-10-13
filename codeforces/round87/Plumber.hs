{-# OPTIONS_GHC -O2 -XBangPatterns #-}

import Data.List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Monad
import Control.Applicative
import Debug.Trace

traceS a b = trace (show a) b

type Z = Int

m = 1000003

--Warning: This program is not correct..

--rowCheck and colCheck returns 0(impossible), 1(restricted), 2(free)

rowCheck :: Z -> Z -> [String] -> Z -> Z
rowCheck n m ls x =
  2 - evenExistRow rowZ - oddExistRow rowZ
  where !rowZ = zip [1..] (ls !! x)

colCheck :: Z -> Z -> [String] -> Z -> Z
colCheck n m ls y =
  2 - evenExistCol colZ - oddExistCol colZ
  where !colZ = zip [1..] $ map (!! y) ls

rowEven '1' = 1
rowEven '2' = 1
rowEven _ =   0

colEven '1' = 1
colEven '4' = 1
colEven _   = 0

evenExistRow [] = 0
evenExistRow ((i,e):rs) 
  | e == '.' = evenExistRow rs
  | even (i+rowEven e) = 1
  | otherwise = evenExistRow rs

oddExistRow [] = 0
oddExistRow ((i,e):rs)
  | e == '.' = oddExistRow rs
  | even (i+1+rowEven e) = 1
  | otherwise = oddExistRow rs

evenExistCol [] = 0
evenExistCol ((i,e):rs)
  | e == '.' = evenExistCol rs
  | even (i+colEven e) = 1
  | otherwise = evenExistCol rs

oddExistCol [] = 0
oddExistCol ((i,e):rs)
  | e == '.' = oddExistCol rs
  | even (i+1+colEven e) = 1
  | otherwise = oddExistCol rs

solve n m ls = foldr modMult 1 $ listR ++ listC
  where listR = [rowCheck n m ls x | x <- [0..(n-1)]]
        listC = [colCheck n m ls y | y <- [0..(m-1)]]
modMult a b = 
  (a * b) `mod` m

main = do
  (n,m) <- (\[a,b] -> (a,b)) . map read . words <$> getLine
  ls <- replicateM n getLine
  print $ solve n m ls
