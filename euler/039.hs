import Data.List
import Debug.Trace 

traceS a = trace $ show a

rightTriangle a b c = a*a + b*b == c*c

triangleNum p = length [1|x<-[1..p], y<-[1..p], p>x+2*y, rightTriangle x y (p-x-y)]

main = print $ ((\(x,y)->y).last.sort) [(triangleNum p, p)|p<-[1..1000]]
