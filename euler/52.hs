import Data.List

same6x n = a && b && c && d && e
  where t = sort.show$n
        a = t == (sort.show.(*2)$n)
        b = t == (sort.show.(*3)$n)
        c = t == (sort.show.(*4)$n)
        d = t == (sort.show.(*5)$n)
        e = t == (sort.show.(*6)$n)

main = print $ head (filter same6x [1..])
