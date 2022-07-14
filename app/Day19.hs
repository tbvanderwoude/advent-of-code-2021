module Day19 where

import Lib
import Parsing
import Data.Char
import Data.List
import Data.Set (Set, fromList, intersection)

type Point = (Int,Int,Int)
data Scanner = S Int [Point]
 deriving Show

parseScanner rs = S id (map (\s -> read ("("++s++")")) $ tail rs)
 where id = read $ filter (isNumber) (head rs) :: Int

getDistanceSet :: Scanner -> Set Int
getDistanceSet (S id ps) = fromList $ filter (0/=) $ concat (map (beaconDsts ps) ps)
beaconDsts ps p1 = map (\p2 -> sqrDst p1 p2) ps
sqrDst (x1,x2,x3) (y1,y2,y3) = (x1-y1)^2 + (x2-y2)^2 + (x3-y3)^2

isSimilar f1 f2 = length (intersection f1 f2) >= 66

main :: IO ()
main = 
  do inp <- readLines "inputs/input19_test.txt" :: IO [String]
     let rawScanners = split "" inp
     let scanners = map parseScanner rawScanners
     print scanners
     let fingerprints = map getDistanceSet scanners
     print fingerprints 
     let h:t = fingerprints
     print h
     let similarities = map (isSimilar h) t
     return ()
