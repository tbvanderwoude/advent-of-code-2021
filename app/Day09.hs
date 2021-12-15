module Day09 where

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

fillGrid points grid = [[if (elem (i,j) points) then 9 else v | (i,v) <- enumerate xs] | (j,xs) <- enumerate grid]

basinSize :: [Point] -> (Grid Int) -> Int
basinSize [] grid = 0
basinSize points grid = (length points) + (basinSize allNeighbours updatedGrid)
 where updatedGrid = fillGrid points grid
       allNeighbours = filter (\p -> (getPoint updatedGrid p) /= 9) $ rmdups (concat (map (getNeighbours grid) points))

getNeighbours :: (Grid Int) -> Point -> [Point]
getNeighbours grid (x,y)= filter (\(x,y) -> x < w && x >= 0 && y < h && y >= 0) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
 where (w,h) = getDims grid


main :: IO ()
main = 
  do inp <- readLines "inputs/input09.txt"
     let numGrid = map (map digitToInt) inp :: Grid Int
     let neighGrid = [[((v, minimum (map (getPoint numGrid) (getNeighbours numGrid (i,j)))),(i,j)) | (i,v) <- enumerate xs] | (j,xs) <- enumerate numGrid] 
     let minima = filter (\((x,y),p) -> x < y) (concat neighGrid)
     let num = length minima
     let p1 = (sum (map (fst . fst) minima)) + num
     let minimaCoords = map snd minima
     print minima
     print p1
     let basins = sort [basinSize [minCoord] numGrid | minCoord <- minimaCoords ]
     -- let basinSizes = [basinSize [mnm] numGrid | mnm <- minima] 
     print (product (take 3 $ reverse basins))
     --print basinSizes
     return () 
