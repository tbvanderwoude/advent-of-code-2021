module Main where

import Lib
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char


type Grid a = [[a]]

type Point = (Int,Int)

getDims grid = (length (head grid), length grid)

getPoint grid (x,y) = (grid !! y) !! x

mapGrid f grid = [[f v | v <- xs] | xs <- grid]


intToChar x = chr (ord '0' + x)


convertRow = map intToChar

showGrid grid = concat [convertRow row ++ "\n"| row <- grid]

processResult grid = showGrid (mapGrid fst grid)

getNeighbours :: (Grid (Int,Bool)) -> Point -> [Point]
getNeighbours grid (x,y)= filter (\(x,y) -> x < w && x >= 0 && y < h && y >= 0) [(x-1,y-1),(x+1,y-1),(x-1,y+1),(x+1,y+1),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
 where (w,h) = getDims grid


lookupIncrement :: [(Point,Int)] -> Point -> Int
lookupIncrement m p = case lookup p m of 
                       (Just v) -> v
                       Nothing -> 0

flashGrid :: (Grid (Int,Bool),Int) -> (Grid (Int,Bool), Int)
flashGrid (grid,counts) = if (null updates) then (mapGrid (\(v,f) -> if f then (0,True) else (v,False)) $ finalGrid, counts + additionalFlashes) else flashGrid (finalGrid,counts + additionalFlashes) 
 where updatedGrid = [[(if (not b && v>9) then getNeighbours grid (i,j) else [], if (not b && v>9) then 0 else v, b || v>9) | (i,(v,b)) <- enumerate xs] | (j,xs) <- enumerate grid] :: Grid ([Point], Int, Bool)
       additionalFlashes = sum (map (\(n,_,_) -> if (not . null $ n) then 1 else 0) (concat updatedGrid)) :: Int
       rawUpdates = concat (map (\(us,_,_) -> us) $ concat updatedGrid) :: [Point]
       updates = (map (\xs -> (head xs, length xs))) .  group . sort $ rawUpdates :: [(Point,Int)]
       finalGrid = [[(v + (lookupIncrement updates (i,j)),f) | (i,(_,v,f)) <- enumerate xs] | (j,xs) <- enumerate updatedGrid] :: Grid (Int,Bool)

update :: (Grid (Int,Bool),Int) -> (Grid (Int,Bool),Int)
update (grid, count) = trace (processResult grid) $ flashGrid (baseGrid, count)
 where baseGrid = mapGrid (\(v,f) -> (v+1,False)) grid 

checkedIteration n grid = if (sum (map (\(v,f) -> if f then 1 else 0 ) (concat updatedGrid)) == 100) then (n+1) else checkedIteration (n+1) updatedGrid
 where (updatedGrid,count) = update (grid,0)

main :: IO ()
main = 
  do inp <- readLines "inputs/input11.txt"
     let numGrid = (map (map (\x -> (digitToInt x, False))) inp,0) :: (Grid (Int,Bool),Int)
     print numGrid
     let res = applyN 100 update $ numGrid
     print res 
     print $ checkedIteration 0 (fst numGrid)
