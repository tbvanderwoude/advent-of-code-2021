module Day18 where

import Lib
import Parsing

-- Snailfish number definition and parsing
data SFNumber = No Int | Pair SFNumber SFNumber
 deriving Eq

instance Show SFNumber where
 show (No x) = show x
 show (Pair x y) = "["++(show x)++","++(show y)++"]"

parseNo :: Parser SFNumber
parseNo = No . read . (\x -> [x]) <$> digit

parser :: Parser SFNumber
parser = do
 _ <- char '['
 x <- parseNo <|> parser
 _ <- char ','
 y <- parseNo <|> parser
 _ <- char ']'
 return (Pair x y)

-- Addition and reduction logic
reduceNumber :: SFNumber -> SFNumber
reduceNumber x = case explode x 0 of 
                      Propagate px _ _ -> reduceNumber px 
		      Id no -> case splitNo no of 
		                    Unsplit no -> no 
				    Split sx -> reduceNumber sx
add = Pair

addReduced x y = reduceNumber $ add x y

data ExResult = Propagate SFNumber Int Int | Id SFNumber  

addRightmost v (No x) = No (x + v)
addRightmost v (Pair x y) = Pair x (addRightmost v y)

addLeftmost v (No x) = No (x + v)
addLeftmost v (Pair x y) = Pair (addLeftmost v x) y

getVal :: SFNumber -> Int
getVal (No v) = v
getVal _ = undefined

explode :: SFNumber -> Int -> ExResult  
explode (Pair x y) d | d >= 4 = Propagate (No 0) (getVal x) (getVal y)
                     | otherwise = case explode x (d+1) of 
		                         Id u -> (case explode y (d+1) of 
					                 Id v -> Id (Pair u v)
							 Propagate no rx ry -> Propagate (Pair (addRightmost rx u) no) 0 ry)
					 Propagate no rx ry -> Propagate (Pair no (addLeftmost ry y)) rx 0
explode (No x) _ = Id (No x)

data SplitResult = Split SFNumber | Unsplit SFNumber

splitNo :: SFNumber -> SplitResult  
splitNo (No x) | x >= 10 = Split $ Pair (No $ floor n) (No $ ceiling n)
               | otherwise = Unsplit $ No x
 where n = (fromIntegral x) / 2
splitNo (Pair x y) = case splitNo x of
                          Unsplit no -> (case splitNo y of 
			                      Split no -> Split (Pair x no)
					      Unsplit no -> Unsplit (Pair x y))
                          Split no -> Split (Pair no y) 

-- High-level logic for doing the snailfish' homework and finding the maximum magnitude for part2
processInput :: String -> SFNumber
processInput s = fst . head $ parse parser s 

test s1 s2 = addReduced (processInput s1) (processInput s2)

calculate [n] = n
calculate (x:(y:zs)) = calculate ((addReduced x y):zs)

mag (No x) = x
mag (Pair x y) = 3 * (mag x) + 2 * (mag y)

main :: IO ()
main = 
  do inp <- map processInput <$> readLines "inputs/input18.txt" :: IO [SFNumber]
     print inp
     let r = calculate inp
     let m = mag r
     print m
     let p = cartProd inp inp 
     print (maximum (map (\(x,y) -> mag (addReduced x y)) p))
     return ()
