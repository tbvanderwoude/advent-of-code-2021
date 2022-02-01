module Day19 where

import Lib
import Parsing
import Data.Char

type Point = (Int,Int,Int)
data Scanner = S Int [Point]
 deriving Show

parseScanner rs = S id (map (\s -> read ("("++s++")")) $ tail rs)
 where id = read $ filter (isNumber) (head rs) :: Int

main :: IO ()
main = 
  do inp <- readLines "inputs/input19_test.txt" :: IO [String]
     let rawScanners = split "" inp
     let scanners = map parseScanner rawScanners
     print scanners
     return ()
