module Main where

import Lib
import Data.List
import Debug.Trace

type Point = (Int,Int)
type Line = (Point,Point)
type Grid = [[Int]]

parsePoint :: String -> Point 
parsePoint s = (read s1, read (tail s2)) 
 where (s1,s2) = break (==',') s

makeLine :: [String] -> Line 
makeLine (p1:(p2:[])) = mapTuple parsePoint (p1,p2)
makeLine _ = undefined

verticalTest :: Line -> Bool
verticalTest (p1,p2) = (fst p1 == fst p2) 

horizontalTest :: Line -> Bool
horizontalTest (p1,p2) = (snd p1 == snd p2)

projectOnX :: Line -> (Int,Int)
projectOnX (p1,p2) = (min x1 x2, max x1 x2)
 where x1 = fst p1
       x2 = fst p2

projectOnY :: Line -> (Int,Int)
projectOnY (p1,p2) = (min y1 y2, max y1 y2) 
 where y1 = snd p1
       y2 = snd p2

constructDiagonalPoints :: Line -> [Point]
constructDiagonalPoints l = zip [x | x <- (if (x > x1) then (reverse [x1..x2]) else [x1..x2])] [y | y <- if (y > y1) then (reverse [y1..y2]) else [y1..y2]]
 where (x1,x2) = projectOnX l
       (y1,y2) = projectOnY l
       (x,y) = fst l

lineToPoints :: Line -> [Point]
lineToPoints l = if verticalTest l then [(x1,y) | y <- [y1..y2]] else if horizontalTest l then [(x,y1) | x <- [x1..x2]] else constructDiagonalPoints l
 where (x1,x2) = projectOnX l
       (y1,y2) = projectOnY l

-- Haskell is poetry. But sometimes it's Vogon poetry
countDoublePlusOverlaps :: [Line] -> Int
countDoublePlusOverlaps lines = length . filter (>=2) . (map length) . group . sort . concat . (map lineToPoints) $ lines

main :: IO ()
main = 
  do inp <- getWords "inputs/input05.txt"
     let splitInput = map ((map head) . (split "->")) inp :: [[String]]
     let lines = map makeLine splitInput 
     let horVertLines = filter (\l -> verticalTest l || horizontalTest l) lines
     print $ countDoublePlusOverlaps horVertLines
     print $ countDoublePlusOverlaps lines
