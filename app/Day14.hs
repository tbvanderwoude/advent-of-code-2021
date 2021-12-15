module Day14 where

import Lib
import Data.List
import Debug.Trace

polymerize _ [x] = [x]
polymerize rules (a:(b:cs)) = case lookup (a,b) rules of 
                                     Just i -> (a:(i:(polymerize rules (b:cs))))
                                     Nothing -> (a:(polymerize rules (b:cs)))

parseRule :: String -> ((Char,Char),Char)
parseRule l = ((i !! 0, i !! 1),o !! 0)
 where (i:(_:(o:[]))) = words l

generatePairs :: String -> [((Char,Char),Integer)]
generatePairs s = count (zip s (tail s)) 

type Count a = [(a,Integer)]

type Polymer = (Count (Char,Char),Count Char)

pairMap rules ((x,y),c) = case lookup (x,y) rules of
                               Just i -> [((x,i),c),((i,y),c)]
                               Nothing -> undefined    

genElem rules ((x,y),c) = case lookup (x,y) rules of 
                               Just i -> [(i,c)]
                               Nothing -> undefined

abstractPolymerize :: [((Char,Char),Char)] -> Polymer -> Polymer 
abstractPolymerize rules (pairs, elements) = (newPairs,newCounts) 
 where newPairs = mergeCounts (concat (map (pairMap rules) pairs))
       newCounts = mergeCounts (elements ++ (concat (map (genElem rules) pairs)))

mergeCounts :: Ord a => Count a -> Count a 
mergeCounts xs = map (\xs -> (fst (head xs),sum (map snd xs))) . groupBy (\(a,b) (c,d) -> a == c) . sortBy (\(a,b) (c,d) -> compare a c) $ xs


count :: Ord a => [a] -> Count a 
count = (map (\xs -> (head xs, fromIntegral $ length xs))) . group . sort 


solve n rules templatePolymer = print (last counts - head counts)
 where (fPairs,fCounts) = (applyN n (abstractPolymerize rules)) templatePolymer
       counts = sort (map snd fCounts) 

main :: IO ()
main = 
  do template : ("" : ruleString) <- readLines "inputs/input14.txt" :: IO [String]
     let rules = map parseRule ruleString
     let templatePolymer = (generatePairs template, count template)
     let (fPairs,fCounts) = (applyN 40 (abstractPolymerize rules)) templatePolymer
     let counts = sort (map snd fCounts) 
     solve 10 rules templatePolymer
     solve 40 rules templatePolymer
     return ()
