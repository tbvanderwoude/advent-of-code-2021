module Main where

import Data.Bits
import Lib

addLists :: Num a => [a] -> [a] -> [a]
addLists xs ys = map (uncurry (+)) (zip xs ys)

repVecAdd :: Num a => [a] -> [[a]] -> [a]
repVecAdd as bss = foldr addLists as bss 

binMap '0' = 0
binMap '1' = 1
binMap c = undefined 

findOxygen :: [[Int]] -> [Int]
findOxygen (x:[]) = x
findOxygen [] = undefined
findOxygen xss = b : findOxygen (map tail (filter (\xs -> b == (head xs)) xss))
        where l = length xss
              b = if sum [head xs | xs <- xss] * 2 >= l then 1 else 0

findScrubber :: [[Int]] -> [Int]
findScrubber (x:[]) = x
findScrubber [] = undefined
findScrubber xss = b : findScrubber (map tail (filter (\xs -> b == (head xs)) xss))
        where l = length xss
              b = if sum [head xs | xs <- xss] * 2 < l then 1 else 0

constructInt (x:xs) = x + 2 * (constructInt xs) 
constructInt [] = 0

binToInt xs = constructInt . reverse $ xs

main :: IO ()
main = 
  do inp <- (map (map binMap)) <$> readLines "inputs/input03.txt"
     let n = length inp :: Int
     let d = length (head inp) :: Int
     let counts = repVecAdd (head inp) (tail inp)
     print (n,d,counts)
     let binary = map (\x -> if x >= (div n 2) then 1 else 0) counts
     let epsilonBin = map (\x -> if x >= (div n 2) then 0 else 1) counts
     let gamma = constructInt (reverse binary) :: Int
     let epsilon = (complement gamma) `mod` (shiftL 2 (d-1))
     print (gamma, epsilon, (epsilon * gamma))
     let oxRating = (binToInt. findOxygen) inp
     let carbRating = (binToInt . findScrubber) inp
     print (oxRating,carbRating,(oxRating * carbRating))
