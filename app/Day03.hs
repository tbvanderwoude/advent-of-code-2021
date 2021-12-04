module Main where

import Data.Bits
import Lib


binMap '0' = 0
binMap '1' = 1
binMap c = undefined 

constructInt (x:xs) = x + 2 * (constructInt xs) 
constructInt [] = 0

binToInt xs = constructInt . reverse $ xs

-- Bool signifies whether to look for O2 or CO2
findRating :: Bool -> [[Int]] -> [Int]
findRating o (x:[]) = x
findRating o [] = undefined
findRating o xss = b : findRating o (map tail (filter (\xs -> b == (head xs)) xss))
        where l = length xss
              b = if o then if sum [head xs | xs <- xss] * 2 >= l then 1 else 0 else if sum [head xs | xs <- xss] * 2 < l then 1 else 0

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
     let oxRating = (binToInt. (findRating True)) inp
     let carbRating = (binToInt . (findRating False)) inp
     print (oxRating,carbRating,(oxRating * carbRating))
