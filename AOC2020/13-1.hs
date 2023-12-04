module Main where

import System.IO
import Data.List
import Data.Maybe

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

getId :: Eq a => a -> [a] -> Int
getId e arr = fromJust $ elemIndex e arr

main :: IO()
main = do
    file <- openFile "13-input.txt" ReadMode
    -- file <- openFile "13-ex.txt" ReadMode
    cont <- hGetContents file
    let [searly, sbus] = lines cont
        earliest = (read :: String -> Int) searly 
        busses = [(read :: String -> Int) b | b <- split ',' sbus, b /= "x" ]
        times = [earliest..earliest+1000]
        tau = getId True [ or [ (mod t b) == 0 | b<-busses ] | t<-times ]
        isthere = map (mod (tau+earliest)) busses
        busid = busses !! getId 0 isthere
    print tau
    print isthere
    print $ busid*tau
    -- print busses

