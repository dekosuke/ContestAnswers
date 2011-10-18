fib = 1:1:zipWith (+) fib (tail fib)
fibm = takeWhile (< 4000000) fib

main = print $ sum (filter (\x->mod x 2 == 0) fibm)
