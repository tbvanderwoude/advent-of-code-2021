module Main where

import Lib
import Data.List
import Debug.Trace

compCost :: Integer -> [Integer] -> Integer
compCost n ns = sum (map (\x -> abs(x-n)) ns)

sumOfNats n = div (n*(n+1)) 2

compAltCosts :: Integer -> [Integer] -> Integer
compAltCosts n ns = sum (map (\x -> sumOfNats (abs (x-n))) ns)

main :: IO ()
main = 
  do inp <- (split ',') <$> readFile "inputs/input07.txt"
     let number = sort $ map read inp :: [Integer]
     let costs = [(i, compCost i number) | i <- [(minimum number)..(maximum number)]]
     let min = minimumBy (\x y -> compare (snd x) (snd y)) costs
     print min
     let modifiedCosts = [(i, compAltCosts i number) | i <- [(minimum number)..(maximum number)]]
     let newMin = minimumBy (\x y -> compare (snd x) (snd y)) modifiedCosts 
     print newMin
