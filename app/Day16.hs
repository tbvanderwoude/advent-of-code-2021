module Day16 where

import Lib
import Data.List
import Debug.Trace
import Data.Char
import Numeric (readHex)

intToBinSeq :: Int -> [Int]
intToBinSeq = (padZeroes 4) . reverse . converter

-- Removes leading padding 
binToInt :: [Int] -> Int
binToInt (0:bs) = binToInt bs 
binToInt bs = backConv . reverse $ bs

backConv (0:bs) = 2 * backConv bs 
backConv (1:bs) = 1 + 2 * backConv bs
backConv [] = 0

padZeroes n xs = (replicate (n-l) 0) ++ xs
 where l = length xs

converter 0 = []
converter n = (n `mod` 2) : (converter (n `div` 2))
charToInt c = fst . head $ readHex [c]

charToBinSeq :: Char -> [Int]
charToBinSeq = intToBinSeq . charToInt

type Binary = [Int]

parsePacket bs | t == 4 = parseValue r 
               | otherwise = 0
 where (v,t,r) = parseHeader bs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = h : (chunk n t)
 where (h,t) = splitAt n xs

wholeChunk :: Int -> [a] -> [[a]]
wholeChunk n xs = filter (\as -> length as == n) . (chunk n) $ xs

parseValue bs = binToInt xs
 where (xs,r) = constructBinStr bs

constructBinStr :: Binary -> (Binary,Binary)
constructBinStr bs | f == 0 = (vs,r1)
                   | f == 1 = (\(x,y) -> (vs ++ x,y)) (constructBinStr r1)
 where ((f:vs),r1) = trace (show bs) splitAt 5 bs 
  

parseHeader :: Binary -> (Int,Int,Binary)
parseHeader bs = (binToInt vNo, binToInt tId,r2)
 where (vNo,r1) = splitAt 3 bs
       (tId,r2) = splitAt 3 r1

main :: IO ()
main = 
  do inp <- head <$> readLines "inputs/input16_test.txt" :: IO String
     print inp
     let x = concat $ map charToBinSeq inp
     print x
     print (parseHeader x)
     print (parsePacket x)
     return ()
