import Data.List
import qualified Data.Text as T
import Debug.Trace 

traceS a = trace $ show a

--cat words.txt | runghc 042.hs

type Z = Int

ts = [div (n*(n+1)) 2| n<-[1..]]

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

al2num a =
  case elemIndex a alphabet of
    Nothing -> (-1)
    Just x -> x+1

tsFind a = tsFind' a ts
tsFind' a (t:tss) 
  | a>t = tsFind' a tss
  | a==t = True
  | otherwise = False

readWords l = map (unQuote.T.unpack) $ T.splitOn (T.pack ",") t
  where t = T.pack l

unQuote l = cut $ cut l
cut l = reverse $ drop 1 l

wScore :: String -> Z
wScore w = sum $ map al2num w

myMain l = show $ sum [1 | w<-ws, tsFind $wScore w]
  where ws = readWords $ l

main = interact myMain
