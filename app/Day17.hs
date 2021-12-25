module Day17 where

import Lib
import Debug.Trace
import Text.Regex.Posix

integerSum :: Int -> Int
integerSum n = n*(n+1) `div` 2

findXVelo target = head (filter (\x -> integerSum x >= target) [0..])

simulate :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
simulate ((x,y),(vx,vy)) = ((x+vx,y+vy),(if vx==0 then 0 else if vx > 0 then vx-1 else vx+1,vy-1))
simulateLim ((x,y),(vx,vy)) (xmin,xmax) (ymin,ymax) = if (x <= xmax && y>=ymin) then (if (x >= xmin &&  y <= ymax) then True else simulateLim newState (xmin,xmax) (ymin,ymax)) else False
 where newState = (if (y==0) then (trace (show (x,y,vx,vy))) else id) $ simulate ((x,y),(vx,vy))

main :: IO ()
main = 
  do inp <- readFile "inputs/input17.txt" :: IO String 
     let [xmin,xmax,ymin,ymax] = (map read) . tail . head $ (inp =~ "target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)") :: [Int]
     print (xmin,xmax,ymin,ymax)
     print (findXVelo xmin)
     let minXVelo = findXVelo xmin
     -- Discrete kinematics!
     let bestYVelo= -ymin - 1
     let maxYHeight = integerSum bestYVelo
     print (minXVelo,bestYVelo, maxYHeight)
     let xVRange = [minXVelo..xmax]
     let yVRange = [ymin..bestYVelo]
     let v = simulateLim ((0,0),(minXVelo,bestYVelo)) (xmin,xmax) (ymin,ymax)
     print v
     -- print (xVRange,yVRange)
     -- let combs = filter (\(vx,vy) -> simulateLim ((0,0),(vx,vy)) (xmin,xmax) (ymin,ymax)) (cartProd xVRange yVRange)
     --print (length combs)

     return ()
