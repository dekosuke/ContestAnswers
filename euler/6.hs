h = [1..100]
sq a = a*a
dif = (sq $ sum h) - (sum $ map sq h)

main = print dif
