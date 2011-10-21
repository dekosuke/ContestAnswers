curious a b
  | c==1 || c==b = False
  | sa0==sb1 && a*sb0==b*sa1 = True
  | sa1==sb0 && a*sb1==b*sa0 = True
  | otherwise = False
  where c = gcd a b
        sa = show a
        sb = show b
        sa0 = read [sa!!0]
        sa1 = read [sa!!1]
        sb0 = read [sb!!0]
        sb1 = read [sb!!1]
           
findProdDenomi xs = rasDenomi $ foldr (\(a0,b0) (a1,b1)->(a0*a1,b0*b1)) (1,1) xs
rasDenomi (a,b) = div b c
  where c = gcd a b

main = print $ findProdDenomi [(a,b)|a<-[10..99],b<-[a+1..99],curious a b]
