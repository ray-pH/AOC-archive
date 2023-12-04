module Main where

import Data.List
import Debug.Trace

type Cup = Int

cycleClockwise :: [a] -> [a]
cycleClockwise (x:xs) = xs ++ [x]

cycleUntil :: Eq a => a -> [a] -> [a]
cycleUntil e (x:xs) 
    | e == x = (x:xs)
    | otherwise = cycleUntil e $ cycleClockwise (x:xs)

getDest :: Cup -> [Cup] -> Cup
getDest cup cups 
    | cup == 0 = getDest 9 cups
    | elem cup cups = cup
    | otherwise = getDest (cup-1) cups

moveCup :: [Cup] -> [Cup]
moveCup cups = let (curr:pickup) = take 4 cups
                   left   = drop 4 cups
                   dest   = getDest (curr-1) left
                   destid = case elemIndex dest left of
                              Nothing -> 0
                              Just val -> val
                   lleft  = take (destid+1) left
                   rleft  = drop (destid+1) left
                   cups'  = lleft ++ pickup ++ rleft ++ [curr]
        -- in trace (show (dest, destid, left, lleft,pickup,rleft,curr)) $ cycleUntil dest cups'
                in cups'

doMultipleTime :: Int -> [Cup] -> [Cup]
doMultipleTime 0 cups = cups
doMultipleTime n cups = doMultipleTime (n-1) (moveCup cups)

main :: IO()
main = do 
    let f     = (\y -> (read :: String -> Int) $ y:"")
        ex1   = map f $ show 32415
        ex2   = map f $ show 389125467
        input = map f $ show 284573961
        final = doMultipleTime 100 input
        outp  = foldr (++) "" $ map show $ tail $ cycleUntil 1 final
    print ex2
    print final
    print outp
