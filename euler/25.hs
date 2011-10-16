import Debug.Trace

traceS a = trace $ show a

fib :: [Integer]
fib = 1:1:zipWith (+) fib (tail fib)

thoudigit :: Integer
thoudigit = 10 ^ 999 - 1

main = print $ length (takeWhile (< thoudigit) fib) + 1
