module Day13 where

import Lib
import Data.List
import Debug.Trace

type Point = (Int,Int)

parsePoint :: String -> Point
parsePoint s = (read a, read b)
 where (a:(b:_)) = split ',' s

data Command = X Int | Y Int
 deriving Show

parseCommand :: String -> Command
parseCommand s | t == 'x' = X (read ts)
               | t == 'y' = Y (read ts)
 where (t:('=':ts)) = last (words s)

-- At last, the elegance of functional programming!
intoMap :: Command -> (Point -> Point)
intoMap (X x0) = \(x1,y1) -> (if x1 > x0 then x0-(x1-x0) else x1,y1)
intoMap (Y y0) = \(x1,y1) -> (x1,if y1 > y0 then y0-(y1-y0) else y1)

renderPoints :: [Point] -> [String] 
renderPoints ps =  [[if (elem (i,j) ps) then '#' else ' ' | i <- [0..w]] | j <- [0..h]]
 where (w,h) = (maximum (map fst ps), maximum (map snd ps)) :: (Int,Int)

main :: IO ()
main = 
  do inp <- readLines "inputs/input13.txt" :: IO [String]
     let (pointStr : (commandStr:_)) = split "" inp
     let ps = map parsePoint pointStr
     let cs = map parseCommand commandStr
     let operator = intoMap (head cs)
     let p1 = length . rmdups $ map operator ps
     print p1
     let totalOp = foldl (\y x-> (intoMap x) . y) id cs
     let finalPoints = rmdups $ map totalOp ps
     sequence $ map print (renderPoints finalPoints)
     return ()
