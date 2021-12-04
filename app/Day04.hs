module Main where

import Lib
import Debug.Trace

type Grid = [[Int]]

split :: Eq a => a -> [a] -> [[a]]
split c xs = case break (==c) xs of
 (ls,[])-> [ls]
 (ls, x:rs) -> ls : split c rs

gridFromList :: [String] -> Grid
gridFromList [] = []
gridFromList (xs:xss) = (map read $ (filter (/="") $ split ' ' xs)) : gridFromList xss

markNumber n grid = [[if x == n then -1 else x | x <- xs ] | xs <- grid]

checkBingo :: Grid -> Bool
checkBingo grid = (any (== -5) (map sum grid)) || (any (== -5) (map sum (transpose grid)))

getScore :: Grid -> Int
getScore grid = sum (filter (/= -1) (concat grid))

main :: IO ()
main = 
  do inp <- readLines "inputs/input04.txt"
     let numbers = map read $ split ',' (head inp) :: [Int]
     let rawGrids = split "" (tail (tail inp)) :: [[String]]
     let grids = map gridFromList rawGrids :: [Grid]
     print $ squidGame numbers grids
     print $ squidGameUntilEnd numbers grids

squidGame [] _ = undefined
squidGame (n:ns) grids = if (not . null $ winningGrids) then (n * (getScore (head winningGrids))) else squidGame ns markedGrids
 where markedGrids = map (markNumber n) grids :: [Grid]
       winningGrids = filter checkBingo markedGrids :: [Grid]

squidGameUntilEnd :: [Int] -> [Grid] -> Int
squidGameUntilEnd ns [g] = squidGame ns [g]
squidGameUntilEnd [] _ = undefined 
squidGameUntilEnd (n:ns) grids = squidGameUntilEnd ns (filter (not . checkBingo) markedGrids)
 where markedGrids = map (markNumber n) grids :: [Grid]
