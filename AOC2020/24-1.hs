module Main where

import System.IO

parseTiles :: String -> [String]
parseTiles "" = []
parseTiles ('s':'e':ss) = "se" : parseTiles ss
parseTiles ('s':'w':ss) = "sw" : parseTiles ss
parseTiles ('n':'e':ss) = "ne" : parseTiles ss
parseTiles ('n':'w':ss) = "nw" : parseTiles ss
parseTiles ('e':ss)     = "e"  : parseTiles ss
parseTiles ('w':ss)     = "w"  : parseTiles ss

type Vector = (Int,Int)

addVector :: Vector -> Vector -> Vector
addVector (x1,y1) (x2,y2) = (x1+x2, y1+y2)

toVector :: String -> Vector 
toVector "se" = ( 1,-1)
toVector "sw" = ( 0,-1)
toVector "ne" = ( 0, 1)
toVector "nw" = (-1, 1)
toVector "e"  = ( 1, 0)
toVector "w"  = (-1, 0)

getPosition :: [Vector] -> Vector
getPosition = foldr addVector (0,0)

flipTile :: Vector -> [Vector] -> [Vector]
flipTile vec blacktiles 
    | elem vec blacktiles = filter (/= vec) blacktiles
    | otherwise           = vec : blacktiles

doFlipping :: [Vector] -> [Vector] -> [Vector]
doFlipping [] after = after
doFlipping (b:bs) after = doFlipping bs $ flipTile b after

main :: IO()
main = do
    file <- openFile "24-input.txt" ReadMode
    -- file <- openFile "24-ex.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        rawtiles = map parseTiles lin
        rvectors = map (map toVector) rawtiles
        vectors  = map getPosition rvectors
        blacks   = doFlipping vectors []
    print vectors
    print blacks
    print $ length blacks
    -- mapM_ print vectors
