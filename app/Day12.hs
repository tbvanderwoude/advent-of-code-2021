module Main where

import Lib
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

type Edge = (String,String)

parseLine :: String -> Edge
parseLine line = (substrings !! 0, substrings !! 1)
 where substrings= (split '-') line
       
lookupNeighbour k t = head [v | (k', v) <- t, k==k']

lowerCase :: String -> Bool
lowerCase = all isLower 

-- Actually constructs all paths for part 1 
generatePaths :: [(String,[String])] -> [String] -> String -> [[String]]
generatePaths _ _ "end" = [["end"]]
generatePaths neighs visited node = concat (map (map (node:)) [(generatePaths neighs (if lowerCase n then (n:visited) else visited) n) | n <- unvisitedNeigh])
 where unvisitedNeigh = filter (\x -> not (elem x visited)) (lookupNeighbour node neighs) :: [String]

-- Directly counts the paths 
countPaths :: [(String,[String])] -> [String] -> String -> Int 
countPaths _ _ "end" = 1 
countPaths neighs visited node = sum [(countPaths neighs (if lowerCase n then (n:visited) else visited) n) | n <- unvisitedNeigh]
 where unvisitedNeigh = filter (\x -> not (elem x visited)) (lookupNeighbour node neighs) :: [String]
                                
main :: IO ()
main = 
  do inp <- (map parseLine) <$> readLines "inputs/input12.txt"
     print inp
     -- Makes all edges except from start or to end bidirectional
     let biDir = concat [if not (u == "start" || v == "end") then [(u,v),(v,u)] else [(u,v)] | (u,v) <- inp]
     -- Constructs per-node neighbour list
     let neighbourMap = [(fst (head ns),map snd ns) | ns <- groupBy (\x y -> fst x == fst y) . sort $ biDir] :: [(String,[String])]
     print neighbourMap
     --let paths = rmdups (generatePaths neighbourMap ["start"] "start")
     -- print (length paths)
     print $ countPaths neighbourMap ["start"] "start"
