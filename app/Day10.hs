module Main where

import Lib
import Control.Monad
import Data.List
import Debug.Trace

opens :: Char -> Bool
opens '(' = True
opens '[' = True
opens '{' = True
opens '<' = True
opens _ = False

closer :: Char -> Char 
closer '(' = ')' 
closer '[' = ']' 
closer '{' = '}' 
closer '<' = '>' 

charScore ')' = 3
charScore ']' = 57 
charScore '}' = 1197 
charScore '>' = 25137 

rateCorruption c1 c2 | c1 == c2 = 0
                     | otherwise = charScore c2

buildStack :: [Char] -> String -> [Char]
buildStack stack [] = stack
buildStack stack (c:s) | opens c = buildStack ((closer c) : stack) s
                       | otherwise = buildStack (tail stack) s
stackScore ')' = 1
stackScore ']' = 2
stackScore '}' = 3
stackScore '>' = 4

rateStack stack = foldl (\x y -> x * 5 + stackScore y) 0 stack

scoreCorruption:: [Char] -> String -> Int 
scoreCorruption _ [] = 0 
scoreCorruption stack (c:s) | opens c = scoreCorruption ((closer c) : stack) s
                            | otherwise = if (not (null stack)) then (if corruption /= 0 then corruption else (scoreCorruption (tail stack) s)) else 0
 where corruption = (rateCorruption (head stack) c) 

main :: IO ()
main = 
  do lines <- readLines "inputs/input10.txt"
     let corruptLines = filter (\x -> 0 /= scoreCorruption [] x) lines
     print (sum (map (scoreCorruption []) lines))
     let incompleteLines = filter (\x -> 0 == scoreCorruption [] x) lines
     let stackScores = sort $ map (rateStack . buildStack []) incompleteLines 
     let middleScore = stackScores !! (div (length stackScores) 2)
     print middleScore
