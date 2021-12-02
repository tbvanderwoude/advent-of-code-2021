module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

mapFile :: (Read a) => FilePath -> IO [a]
mapFile path = (map read) <$> readLines path

getWords path = (map words) <$> readLines path

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
