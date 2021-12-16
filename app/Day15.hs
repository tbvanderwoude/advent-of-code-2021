module Day15 where

import Lib
import Data.List
import Debug.Trace
import qualified Data.Set as Set
import Data.Char
import Data.Graph
import qualified Data.PQueue.Min as PQ

type Grid a = [[a]]
type Point = (Int,Int)

type Path = [(Int,Int)]
type Cost = Int
data Node = N Point Cost Path

instance Eq Node where
 (==) (N _ c1 _) (N _ c2 _) = c1 == c2
instance Ord Node where
 compare (N _ c1 _) (N _ c2 _) = compare c1 c2


getNeighbours :: (Grid a) -> Point -> [Point]
getNeighbours grid (x,y)= filter (\(x,y) -> x < w && x >= 0 && y < h && y >= 0) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
 where (w,h) = getDims grid

getDims grid = (length (head grid), length grid)

constructGridGraph :: Grid Int -> (Graph, Point -> (Int, Point, [Point]))
constructGridGraph grid = (graph, buildLink nodeFromVertex vertexFromKey) 
 where edges = concat [[(v,(i,j),getNeighbours grid (i,j)) | (i,v) <- enumerate xs] | (j,xs) <- enumerate grid] :: [(Int,Point,[Point])]
       (graph,nodeFromVertex,vertexFromKey) = graphFromEdges edges

-- This is partial but no illegal key lookups should happen in practice
buildLink nodeFromVertex vertexFromKey key = nodeFromVertex vertex
 where (Just vertex) = vertexFromKey key 

dijkstra graph nodeFromKey startNode goal = dijkstraIter graph nodeFromKey startPQ visited goal
 where startPQ = PQ.singleton startNode :: PQ.MinQueue Node 
       visited = Set.empty :: Set.Set Point


dijkstraIter graph nodeFromKey pq visited goal = if (point==goal) then c else if (Set.member point visited) then (dijkstraIter graph nodeFromKey reducedPq visited goal) else (dijkstraIter graph nodeFromKey newPq newVisited goal)
 where (N point c path, reducedPq) = PQ.deleteFindMin pq
       (incr, _, neighbours) = nodeFromKey point :: (Int, Point, [Point])
       newVisited = Set.insert point visited 
       unvisitedNeighbours = filter (\x -> Set.notMember x newVisited) neighbours 
       lookups = map nodeFromKey unvisitedNeighbours 
       newNodes = map (\(c2, n, _) -> N n (c+c2) []) lookups 
       newPq = foldr PQ.insert reducedPq newNodes
       
main :: IO ()
main = 
  do inp <- readLines "inputs/input15.txt" :: IO [String]
     let grid = map (map digitToInt) inp :: Grid Int     
     print grid  
     let (graph, nodeFromKey) = constructGridGraph grid
     let startNode = N (0,0) 0 []
     let (w,h) = getDims grid
     print (w,h)
     let solution = dijkstra graph nodeFromKey startNode (w-1,h-1)
     print solution
     return ()
