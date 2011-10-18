num :: Integer
num = mod (28433 * (getNum 1 7830457) + 1) 10000000000

getNum a 0 = a
getNum !a !b = getNum (mod (a*2) 10000000000) (b-1)

main = print num
