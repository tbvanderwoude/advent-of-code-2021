module Day15 where

import Lib
import Data.List
import Debug.Trace
import Data.PSQueue

main :: IO ()
main = 
  do inp <- readLines "inputs/input15.txt" :: IO [String]
     print inp
     return ()
