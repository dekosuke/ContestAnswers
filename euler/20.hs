fact 0 = 1
fact n = fact (n-1) * n

main = print $ sum (map (\x -> read [x]) ((show.fact) 100))
