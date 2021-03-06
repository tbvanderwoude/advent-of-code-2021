module Lib where

addLists :: Num a => [a] -> [a] -> [a]
addLists = combineLists (uncurry (+))

subLists = combineLists (uncurry (-))

applyN :: Int -> (a -> a) -> (a -> a)
applyN n f = foldr1 (.) (replicate n f)

repVecAdd :: Num a => [a] -> [[a]] -> [a]
repVecAdd = foldr addLists 

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

addVec (xs:xss) = repVecAdd xs xss
addVec [] = undefined

combineLists f xs ys = map f (zip xs ys)


enumerate = zip [0..]

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f t = (f (fst t), f (snd t))

transpose :: [[a]] -> [[a]]
transpose xss = [[(xss !! j) !! i | (j,_) <- enumerate xs] | (i,xs) <- enumerate xss] 

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

mapFile :: (Read a) => FilePath -> IO [a]
mapFile path = (map read) <$> readLines path

getWords path = (map words) <$> readLines path

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

split :: Eq a => a -> [a] -> [[a]]
split c xs = case break (==c) xs of
 (ls,[])-> [ls]
 (ls, x:rs) -> ls : split c rs

