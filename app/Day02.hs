module Main where

import Lib

commands :: [[String]] -> [(String,Int)]
commands xss = map (\xs -> (head xs, read (head (tail xs)))) xss

folder :: (Int,Int) -> (String, Int) -> (Int,Int)
folder (x,y) ("forward",n) = (x + n, y)
folder (x,y) ("down", n) = (x, y + n)
folder (x,y) ("up", n) = (x, y - n)

folder2 :: (Int,Int,Int) -> (String,Int) -> (Int,Int,Int)
folder2 (x,y,a) ("forward",n) = (x + n, y + (a * n), a)
folder2 (x,y,a) ("down", n) = (x, y, a + n)
folder2 (x,y,a) ("up", n) = (x, y, a - n)

main :: IO ()
main = 
  do inp <- commands <$> getWords "inputs/input02.txt"
     let (x1,y1) = foldl folder (0,0) inp
     print (x1 * y1)
     let (x2,y2,aim) = foldl folder2 (0,0,0) inp :: (Int,Int,Int)
     print (x2 * y2)
