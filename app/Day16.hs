module Day16 where

import Lib
import Data.List
import Debug.Trace
import Data.Char
import Numeric (readHex)
import Parsing

import Data.Char (digitToInt)
import Data.List (foldl')

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

intToBinSeq :: Int -> [Int]
intToBinSeq = (padZeroes 4) . reverse . converter

backConv (0:bs) = 2 * backConv bs 
backConv (1:bs) = 1 + 2 * backConv bs
backConv [] = 0

padZeroes n xs = (replicate (n-l) 0) ++ xs
 where l = length xs

converter 0 = []
converter n = (n `mod` 2) : (converter (n `div` 2))
charToInt c = fst . head $ readHex [c]

intToBin 0 = '0'
intToBin 1 = '1'
intToBin _ = undefined

charToBinSeq :: Char -> String 
charToBinSeq = (map intToBin) . intToBinSeq . charToInt

data Packet = ValPacket Int Int | OpPacket Int Int [Packet]
 deriving Show


versionSum (ValPacket v _) = v
versionSum (OpPacket v t ps) = v + sum (map versionSum ps)

evalAll = map evaluate 

typeToFunc :: Int -> ([Int] -> Int)
typeToFunc t = case t of 
             0 -> sum
             1 -> product
	     2 ->  minimum
	     3 -> maximum 
	     5 -> \xs -> if (xs !! 0) > (xs !! 1) then 1 else 0
	     6 -> \xs -> if (xs !! 0) < (xs !! 1) then 1 else 0 
	     7 -> \xs -> if (xs !! 0) == (xs !! 1) then 1 else 0
	     _ -> head 
	    
evaluate (ValPacket _ v) = v
evaluate (OpPacket v t ps) = f recEvals 
 where recEvals = map evaluate ps :: [Int]
       f = typeToFunc t :: ([Int] -> Int)

takeN n = sequence (replicate n item)

parseBinNum :: Int -> Parser Int
parseBinNum n = do s <- takeN n
                   return (toDec s)

parseChunks :: Parser String 
parseChunks = do s <- takeN 5 
                 if (head s) == '1' then ((tail s)++) <$> parseChunks else return (tail s)
                     
parseValue :: Int -> Parser Packet
parseValue v = do rawValue <- parseChunks 
                  return $ ValPacket v (toDec rawValue) 

limitParser :: Parser a -> Int -> Parser a
limitParser p n = P (\s -> limitHelper p n s)

limitHelper :: Parser a -> Int -> String -> [(a,String)] 
limitHelper p n s = map (\(x,y) -> (x,y++addRem)) parseResult
 where (toParse,addRem) = splitAt n s
       parseResult = parse p toParse 

parseOperator :: Int -> Int -> Parser Packet 
parseOperator v t = do lType <- item 
                       packets <- (case lType of 
		                  '0' -> (do length <- parseBinNum 15
		                             limitParser (many parsePacket) length)
		                  '1' -> (do no <- parseBinNum 11 
		                             sequence (replicate no parsePacket)))
                       return $ OpPacket v t packets


parsePacket :: Parser Packet 
parsePacket = do version <- parseBinNum 3 
                 typeId <- parseBinNum 3
		 case typeId of 
		  4 ->  parseValue version
		  _ -> parseOperator version typeId


firstParse x = head (parse parsePacket $ concat $ map charToBinSeq x)

main :: IO ()
main = 
  do inp <- head <$> readLines "inputs/input16.txt" :: IO String
     print inp
     let packets = fst . firstParse $ inp
     print (versionSum packets) 
     print (evaluate packets)
     return ()
