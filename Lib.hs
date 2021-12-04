module Lib where

addLists :: Num a => [a] -> [a] -> [a]
addLists xs ys = map (uncurry (+)) (zip xs ys)

repVecAdd :: Num a => [a] -> [[a]] -> [a]
repVecAdd as bss = foldr addLists as bss 

addVec (xs:xss) = repVecAdd xs xss
addVec [] = undefined

enumerate xs = zip [0..] xs

transpose :: [[a]] -> [[a]]
transpose xss = [[(xss !! j) !! i | (j,_) <- enumerate xs] | (i,xs) <- enumerate xss] 

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

mapFile :: (Read a) => FilePath -> IO [a]
mapFile path = (map read) <$> readLines path

getWords path = (map words) <$> readLines path

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
