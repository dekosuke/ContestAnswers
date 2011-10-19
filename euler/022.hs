import Data.List
import qualified Data.Text as T
import Debug.Trace 

traceS a = trace $ show a

--cat words.txt | runghc 042.hs

type Z = Int

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

al2num a =
  case elemIndex a alphabet of
    Nothing -> (-1)
    Just x -> x+1

readWords l = map (unQuote.T.unpack) $ T.splitOn (T.pack ",") t
  where t = T.pack l

unQuote l = cut $ cut l
cut l = reverse $ drop 1 l

wScore :: String -> Z
wScore w = sum $ map al2num w

myMain l = show $ sum [(i+1) * wScore (ws !! i) | i<-[0..length ws - 1]]
  where ws = sort $ readWords l

main = interact myMain
