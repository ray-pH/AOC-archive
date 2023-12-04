module Main where

import System.IO
import Data.List
import Data.Maybe

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

getId :: Eq a => a -> [a] -> Int
getId e arr = fromJust $ elemIndex e arr


doInc :: Integer -> [Integer] -> [Integer]
doInc inc = map (+ inc)

theMods :: Integer -> [Integer] -> [Integer] -> [Integer]
theMods t ts ids = [ mod (t+tt) id | (tt,id) <- zipped  ] 
    where zipped = zip ts ids

isCorrect :: Integer -> [Integer] -> [Integer] -> Bool
isCorrect t ts ids = and $ map (== 0) $ theMods t ts ids

main :: IO()
main = do
    file <- openFile "13-input.txt" ReadMode
    -- file <- openFile "13-ex.txt" ReadMode
    cont <- hGetContents file
    let [_, sbus] = lines cont
    -- let sbus = "1789,37,47,1889"
        raw = [((read :: String -> Integer) b, i) | (b,i) <- zip (split ',' sbus) [0..], b /= "x" ]
        busses = [ b | (b,i) <- raw ]
        ts = [ i | (b,i) <- raw ]
        fs = [ (\y -> mod (y + i) b)  | (b,i) <- raw ]
        fid = head busses
        -- times :: [Integer]
        -- times = [1889*i - 3 | i<-[0..]]
        -- tim = head $ [tt | (tt,b) <- [ (t , and $ map (== 0) [ f t | f <- fs ]) | t <- times], b]
        -- tim = head $ filter (\y -> isCorrect y ts busses) ([457*i - 50 | i<-[0..]])
        tim = head $ filter (\y -> isCorrect y ts busses) ([fid*i | i<-[0..]])
        -- times = [earliest..earliest+1000]
        -- tau = getId True [ or [ (mod t b) == 0 | b<-busses ] | t<-times ]
        -- isthere = map (mod (tau+earliest)) busses
        -- busid = busses !! getId 0 isthere
    print raw
    print busses
    print ts
    print tim

