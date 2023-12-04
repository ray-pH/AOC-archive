module Main where

import Data.List
import Data.Maybe

turn :: [Int] -> [Int]
turn (x:xs) 
  | elem x xs = (fromJust $ elemIndex x xs) + 1 : x : xs 
  | otherwise = 0 : x : xs

iter :: Int -> [Int] -> [Int]
iter lim arr 
  | length arr == lim = arr
  | otherwise = iter lim $ turn arr

main :: IO()
main = do
    let ex1 = "0 3 6"
        ex2 = "1 3 2"
        ex3 = "3 1 2"
        inp = "17 1 3 16 19 0"
        input = reverse $ map (read :: String -> Int)$ words inp
        iterated = iter 2020 input
    -- print iterated
    print $ head iterated
