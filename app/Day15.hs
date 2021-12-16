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
type Heuristic = Int
data Node = N Point Cost Heuristic

instance Eq Node where
 (==) (N _ c1 h1) (N _ c2 h2) = (c1 + h1) == (c2 + h2)
instance Ord Node where
 compare (N _ c1 h1) (N _ c2 h2) = compare (c1 + h1) (c2 + h2)


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

heuristic (x1,y1) (x2,y2) = ((abs (x1 - x2)) + (abs (y1 - y2)))

dijkstraIter graph nodeFromKey pq visited goal = if (point==goal) then c else if (Set.member point visited) then (dijkstraIter graph nodeFromKey redPq visited goal) else (dijkstraIter graph nodeFromKey newPq newVisited goal)
 where (N point c h, redPq) = PQ.deleteFindMin pq :: (Node, PQ.MinQueue Node)
       (_ , _, neighbours) = nodeFromKey point :: (Int, Point, [Point])
       newVisited = Set.insert point visited 
       unvisitedNeighbours = filter (\x -> Set.notMember x newVisited) neighbours 
       newNodes = map (\(c2, n, _)  -> N n (c+c2) (heuristic n goal)) (map nodeFromKey unvisitedNeighbours) :: [Node]
       newPq = PQ.union redPq (PQ.fromList newNodes)


expandGrid grid = concat (buildGrids [(concat (buildRows xs)) | xs <- grid] )

buildGrids :: [[Int]] -> [[[Int]]]
buildGrids genGrid = [[[wrapper i v | v <- xs] | xs <- genGrid] | i <- [0..4]]
 
wrapper :: Int -> Int -> Int
wrapper i x = if x + i > 9 then (x+i) - 9 else x + i 

buildRows :: [Int] -> [[Int]]
buildRows genRow = [map (wrapper i) genRow  | i <- [0..4]]



solveGrid grid = dijkstra graph nodeFromKey startNode (w-1,h-1)
 where (graph, nodeFromKey) = constructGridGraph grid
       startNode = N (0,0) 0 0
       (w,h) = getDims grid
       
main :: IO ()
main = 
  do inp <- readLines "inputs/input15.txt" :: IO [String]
     let grid = map (map digitToInt) inp :: Grid Int     
     print (solveGrid grid)
     let expGrid = expandGrid grid
     print (getDims expGrid)
     print (solveGrid expGrid)
     return ()
