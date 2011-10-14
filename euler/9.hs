main = print $ head
   [x*y*(1000-x-y)|x<-[1..400], y<-[x+1..500], x*x+y*y==(1000-x-y)*(1000-x-y)]
