#!/usr/bin/runhaskell
import System.IO
import Text.Printf

parseLine :: String -> [Float]
parseLine = map read . init . words

getPercent :: [Float] -> [Float]
getPercent (n:gold:silver:[]) = [n, 100*gold/total, 100*silver/total]
    where total = gold+silver

toLine :: [Float] -> String
toLine (n:gold:silver:[]) = printf "%2.0f  %5.2f%% %5.2f%%" n gold silver

main :: IO()
main = do
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
    putStr "Day Gold    Siver\n"
    putStr "-----------------\n"
    putStr $ unlines $ map toLine $ map getPercent $ map parseLine lin
