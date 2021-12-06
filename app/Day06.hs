module Main where

import Lib
import Data.List
import Debug.Trace

simulateN :: Int -> [(Int,Int)] -> [(Int,Int)]
simulateN n = (foldr1 (.) (take n (repeat simulate)))

genLogic (timer,count) = if timer == 0 then [(6,count),(8,count)] else [(timer-1,count)]

simulate :: [(Int,Int)] -> [(Int,Int)]
simulate timerCounts = cleanOutput . concat $ (map genLogic timerCounts)

cleanOutput timerCounts = (map (\tuples -> (fst (head tuples), sum (map snd tuples)))) . groupBy (\x y -> fst x == fst y) . sort $ timerCounts

main :: IO ()
main = 
  do inp <- (split ',') <$> readFile "inputs/input06.txt"
     let ns = (map (\xs -> (head xs, length xs))) . group . sort $ (map read) inp :: [(Int,Int)]
     print ns 
     print (sum (map snd (simulateN 80 ns)))
     print (sum (map snd (simulateN 256 ns)))
