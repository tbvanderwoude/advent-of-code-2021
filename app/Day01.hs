module Day01 where

import Lib

threeZip as bs cs = map (\(x,y) -> (x,fst y, snd y)) (zip as (zip bs cs))

listEdges xs = zip xs (tail xs)

main :: IO ()
main = 
  do inp <- mapFile "inputs/input01.txt" :: IO [Int]
     let pairs = filter (\(x,y) -> x < y) $ listEdges inp 
     print (length pairs)
     let triples = threeZip inp (tail inp) (tail (tail inp)) 
     let tripleSum = map (\(x,y,z) -> x + y + z) triples
     print (length (filter (\(x,y) -> x < y) $ listEdges tripleSum))
