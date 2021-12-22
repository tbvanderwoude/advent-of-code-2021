module Day16 where

import Lib
import Numeric (readHex)
import Parsing
import Data.Char
import Data.Tuple
import Data.List 

-- Basic conversions between strings and hexadecimal/binary/decimal numbers
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

intToBinSeq :: Int -> [Int]
intToBinSeq = (padZeroes 4) . reverse . converter

padZeroes n xs = (replicate (n-l) 0) ++ xs
 where l = length xs

converter = unfoldr (\n -> if n == 0 then Nothing else Just (swap $ quotRem n 2)) 

charToInt c = fst . head $ readHex [c]

charToBinSeq :: Char -> String 
charToBinSeq = (map intToBin) . intToBinSeq . charToInt

intToBin 0 = '0'
intToBin 1 = '1'
intToBin _ = undefined

-- Some helper functions for parsing

takeN = parseN item 

parseN p n = sequence (replicate n p) 

parseBinNum :: Int -> Parser Int
parseBinNum n = toDec <$> takeN n

limitParser :: Parser a -> Int -> Parser a
limitParser p n = P (\s -> limitHelper p n s)

limitHelper :: Parser a -> Int -> String -> [(a,String)] 
limitHelper p n s = map (\(x,y) -> (x,y++addRem)) $ parse p toParse 
 where (toParse,addRem) = splitAt n s

parseChunks :: Parser String 
parseChunks = do s <- takeN 5 
                 if (head s) == '1' then ((tail s)++) <$> parseChunks else return (tail s)

-- Packets and packet-specific parsing

data Packet = ValPacket Int Int | OpPacket Int Int [Packet]
 deriving Show

parseOperator :: Int -> Int -> Parser Packet 
parseOperator v t = do lType <- item 
                       (OpPacket v t) <$> (case lType of 
		                  '0' -> (parseBinNum 15 >>= limitParser (many parsePacket))
		                  '1' -> (parseBinNum 11 >>= parseN parsePacket))
                   
parseValue :: Int -> Parser Packet
parseValue v = (ValPacket v) . toDec <$> parseChunks 

parsePacket :: Parser Packet 
parsePacket = do version <- parseBinNum 3 
                 typeId <- parseBinNum 3
		 case typeId of 
		  4 ->  parseValue version
		  _ -> parseOperator version typeId

firstParse = head . parse parsePacket . concat . map charToBinSeq

-- Two ways to evaluate the packet hierarchy

versionSum (ValPacket v _) = v
versionSum (OpPacket v t ps) = v + sum (map versionSum ps)

typeToFunc :: Int -> ([Int] -> Int)
typeToFunc t = case t of 
             0 -> sum
             1 -> product
	     2 -> minimum
	     3 -> maximum 
	     5 -> f (>) 
	     6 -> f (<) 
	     7 -> f (==) 
	     _ -> head 
 where f op = \xs -> if (op (xs !! 0) (xs !! 1)) then 1 else 0
	    
evaluate (ValPacket _ v) = v
evaluate (OpPacket _ t ps) = f recEvals 
 where recEvals = map evaluate ps :: [Int]
       f = typeToFunc t :: ([Int] -> Int)

main :: IO ()
main = 
  do packets <- (fst . firstParse . head) <$> readLines "inputs/input16.txt" :: IO Packet
     print (versionSum packets) 
     print (evaluate packets)
     return ()
