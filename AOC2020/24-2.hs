module Main where

import System.IO
import Debug.Trace

parseTiles :: String -> [String]
parseTiles "" = []
parseTiles ('s':'e':ss) = "se" : parseTiles ss
parseTiles ('s':'w':ss) = "sw" : parseTiles ss
parseTiles ('n':'e':ss) = "ne" : parseTiles ss
parseTiles ('n':'w':ss) = "nw" : parseTiles ss
parseTiles ('e':ss)     = "e"  : parseTiles ss
parseTiles ('w':ss)     = "w"  : parseTiles ss

type Vector = (Int,Int)
type BlackTile = Vector

addVector :: Vector -> Vector -> Vector
addVector (x1,y1) (x2,y2) = (x1+x2, y1+y2)

toVector :: String -> Vector 
toVector "se" = ( 1,-1)
toVector "sw" = ( 0,-1)
toVector "ne" = ( 0, 1)
toVector "nw" = (-1, 1)
toVector "e"  = ( 1, 0)
toVector "w"  = (-1, 0)

allDirection :: [Vector]
allDirection = map toVector ["se","sw","ne","nw","e","w"]

getPosition :: [Vector] -> Vector
getPosition = foldr addVector (0,0)

flipTile :: Vector -> [Vector] -> [BlackTile]
flipTile vec blacktiles 
    | elem vec blacktiles = filter (/= vec) blacktiles
    | otherwise           = vec : blacktiles

doFlipping :: [Vector] -> [BlackTile] -> [BlackTile]
doFlipping [] blacks = blacks
doFlipping (t:ts) blacks = doFlipping ts $ flipTile t blacks

getAdjacents :: BlackTile -> [Vector]
getAdjacents b = map (addVector b) allDirection

getAllAdjacents :: [BlackTile] -> [Vector]
getAllAdjacents [] = []
getAllAdjacents (b:bs) = getAdjacents b ++ getAllAdjacents bs

type Field = (Vector,Int)

isInFields :: Vector -> [Field] -> Bool
isInFields v fields = elem v [ x | (x,_) <- fields ]

appendField :: Vector -> [Field] -> [Field]
appendField vec fields
    | isInFields vec fields = [ if v == vec then incField (v,i) else (v,i) | (v,i) <- fields ]
    | otherwise = (vec,1) : fields 
    where incField :: Field -> Field
          incField (v,i) = (v,i+1)

genField :: [BlackTile] -> [Field]
genField bts = let adjs = getAllAdjacents bts
                in foldr appendField [] adjs

genFieldState :: [BlackTile] -> [(Bool,Field)]
genFieldState bts = let fields = genField bts
                     in [ (elem v bts, (v,i)) | (v,i) <- fields ] 

-- rule isBlack neighboor
rule :: Bool -> Int -> Bool
rule True n = if (n == 0) || (n > 2) then False else True
rule False n = if (n == 2) then True else False

newBlack :: [(Bool,Field)] -> [BlackTile]  
newBlack states = [ v | (b,(v,i)) <- states, rule b i ] 

doCycle :: Int -> [BlackTile] -> [BlackTile]
doCycle 0 bts = bts
doCycle n bts = doCycle (n-1) $ trace ((show n) ++ " " ++ (show $ length nbs)) nbs
    where nbs = newBlack $ genFieldState bts

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
        final    = doCycle 100 blacks
    print vectors
    print blacks
    -- print $ length blacks
    print $ length final 
    -- mapM_ print vectors
