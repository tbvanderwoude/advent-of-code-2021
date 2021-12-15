module Day12 where

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
generatePaths :: String -> [(String,[String])] -> [String] -> String -> [[String]]
generatePaths _ _ _ "end" = [["end"]]
generatePaths double neighs visited node = rmdups (concat (map (map (node:)) [(generatePaths double neighs (if lowerCase n then (n:visited) else visited) n) ++ (if n == double then (generatePaths "" neighs visited n) else []) | n <- unvisitedNeigh]))
 where unvisitedNeigh = filter (\x -> not (elem x visited)) (lookupNeighbour node neighs) :: [String]

-- Directly counts the paths 
countPaths :: [(String,[String])] -> [String] -> String -> Int 
countPaths _ _ "end" = 1 
countPaths neighs visited node = sum [(countPaths neighs (if lowerCase n then (n:visited) else visited) n) | n <- unvisitedNeigh]
 where unvisitedNeigh = filter (\x -> not (elem x visited)) (lookupNeighbour node neighs) :: [String]

countPaths2 :: Int -> [(String,[String])] -> [String] -> String -> Int 
countPaths2 _ _ _ "end" = 1 
countPaths2 double neighs visited node = if smallerCaveVisit && double == 1 then 0 else sum [(countPaths2 (if smallerCaveVisit then double + 1 else double) neighs (node:visited) n) | n <- localNeighs]
 where smallerCaveVisit = lowerCase node && (elem node visited)
       newVisited = (node:visited)
       localNeighs = lookupNeighbour node neighs :: [String]                               


main :: IO ()
main = 
  do inp <- (map parseLine) <$> readLines "inputs/input12.txt"
     print inp
     -- Makes all edges except from start or to end bidirectional
     let biDir = concat [if not (u == "start" || v == "end") then [(u,v),(v,u)] else [(u,v)] | (u,v) <- inp]
     -- Constructs per-node neighbour list
     let neighbourMap = [(fst (head ns),filter ("start"/=) $ map snd ns) | ns <- groupBy (\x y -> fst x == fst y) . sort $ biDir] :: [(String,[String])]
     print neighbourMap
     let smallCaves = filter (\x -> lowerCase x && x /= "start" && x /= "end") (map fst neighbourMap) :: [String]
     print smallCaves
     print $ countPaths neighbourMap ["start"] "start"
     print $ countPaths2 0 neighbourMap [] "start"
     
