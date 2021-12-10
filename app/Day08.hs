module Main where

import Lib
import Control.Monad
import Data.List
import Debug.Trace

type Mapping = [(Char,Integer)]
type Pattern = [Integer]

mappings :: [Mapping]
mappings = map (\x -> zip ['a'..'g'] x) (permutations [1..7])

patterns = [([1,2,3,4,5,6],0),([2,3],1),([1,2,4,5,7],2),([1,2,3,4,7],3),([2,3,6,7],4),([1,3,4,6,7],5),([1,3,4,5,6,7],6),([1,2,3],7),([1,2,3,4,5,6,7],8),([1,2,3,4,6,7],9)]

wordToPattern :: Mapping -> String -> Maybe [Integer]
wordToPattern mapping word = sort <$> sequence [lookup c mapping | c <- word ]

matchWord :: Mapping -> String -> Maybe Int 
matchWord mapping word = do pattern <- wordToPattern mapping word
                            lookup pattern patterns

testMapping :: Mapping -> [String] -> Maybe [Int]
testMapping mapping words = (mapM (matchWord mapping) words)

criterion (Just m) = True
criterion Nothing = False

uberMap :: [String] -> [Maybe (Mapping,[Int])]
uberMap words = map (\m -> (\x -> (m,x)) <$> testMapping m words) mappings

findMapping :: [String] -> Maybe (Mapping,[Int])
findMapping words = head (filter criterion $ uberMap words) 

constructNumber (n:ns) = n + 10 * constructNumber ns
constructNumber [] = 0

solveLine :: ([String],[String]) -> Maybe Int 
solveLine (pattern,output) = do (mapping,_) <- findMapping pattern
                                (constructNumber . reverse) <$> testMapping mapping output

parseLine line = (pattern,output)
 where splitLine = split '|' line
       (patternString,outputString) = (splitLine !! 0 , splitLine !! 1)
       pattern = map sort $ words patternString
       output = map sort $ words outputString

countTrivials :: ([String],[String]) -> Int
countTrivials l = length $ filter (\x -> x /= 5 && x /= 6) $ map length (snd l)


main :: IO ()
main = 
  do inp <- (map parseLine) <$> readLines "inputs/input08.txt"
     print $ countTrivials (head inp)
     let p1 = sum (map countTrivials inp)
     print p1
     print (sum <$> mapM solveLine inp)
     
