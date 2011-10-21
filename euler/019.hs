days     = [31,28,31,30,31,30,31,31,30,31,30,31]
leapdays = [31,29,31,30,31,30,31,31,30,31,30,31]

getAnsYear year moz = getAnsYear' moz ds
  where isLeap = mod year 4 == 0 
        ds = if isLeap then leapdays else days

getAnsYear' _ [] = 0
getAnsYear' 0 (ds1:ds) = 1 + getAnsYear' (mod ds1 7) ds
getAnsYear' k (ds1:ds) = getAnsYear' (mod (k+ds1) 7) ds

getAns 2001 _ = 0
getAns year moz = getAns (year+1) (mod (moz+dayNum) 7) + getAnsYear year moz
  where isLeap = mod year 4 == 0 
        dayNum = if isLeap then 366 else 365

main = print $ getAns 1901 3
