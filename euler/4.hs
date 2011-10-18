import Data.List

pal a = reverse a' == a'
  where a' = show a

main = print $ (head.reverse) (sort [x*y|x<-[100..999], y<-[100..999], pal$x*y])
