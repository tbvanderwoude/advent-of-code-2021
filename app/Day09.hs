module Main where

import Lib
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char


type Grid a = [[a]]

type Point = (Int,Int)

getDims :: (Grid Int) -> (Int,Int)
getDims grid = (length (head grid), length grid)

getPoint grid (x,y) = (grid !! y) !! x

mapGrid f grid = [[f v | v <- xs] | xs <- grid]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

showGrid grid = (map (\x-> concat (map show x)) grid)

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
  do inp <- readLines "inputs/input09_test.txt"
     let numGrid = map (map digitToInt) inp :: Grid Int
     let neighGrid = [[(v, minimum (map (getPoint numGrid) (getNeighbours numGrid (i,j)))) | (i,v) <- enumerate xs] | (j,xs) <- enumerate numGrid] 
     let minima = filter (\(x,y) -> x < y) (concat neighGrid)
     let num = length minima
     let p1 = (sum (map fst minima)) + num
     print minima
     print p1
     print (basinSize [head minima] numGrid)
     sequence (map print $ showGrid numGrid)
     return () 
