module Main where

import System.IO
import Data.List
import Data.Maybe

data Rule = Rule { domain1 :: (Int,Int)
                 , domain2 :: (Int,Int)
                 } deriving Show

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

sdomToDom :: String -> (Int,Int)
sdomToDom str = (x,y)
    where [x,y] = map (read :: String -> Int) $ split '-' str

getRule :: String -> Rule
getRule s = let [_,rhs] = split ':' s
                [sdom1,_,sdom2] = words $ tail rhs
                dom1 = sdomToDom sdom1
                dom2 = sdomToDom sdom2
             in Rule dom1 dom2 

check :: Int -> Rule -> Bool
check n rule = ( min1 <= n && n <= max1 ) || ( min2 <= n && n <= max2 )
    where (min1, max1) = domain1 rule
          (min2, max2) = domain2 rule

checkAllRule :: Int -> [Rule] -> Bool
checkAllRule n rules = or [ check n r | r <- rules ]
-- True if follow at least one rule

-- isValid :: [Int] -> [Rule] -> Bool
-- isValid ticket rules = and [ or [ check i r | r <- rules  ] | i <- ticket ]

customHead :: [Int] -> Int
customHead [] = 0
customHead s = head s

getWrong :: [Int] -> [Rule] -> Int
getWrong ticket rules = customHead $ filter (\y -> not $ checkAllRule y rules) ticket
                   
isValidT :: [Int] -> [Rule] -> Bool
isValidT ticket rules = and $ map (\y -> checkAllRule y rules) ticket

getFromCol :: Int -> [[Int]] -> [Int]
getFromCol col tickets = map (\y -> y !! col) tickets

checkCol :: Int -> [[Int]] -> Rule -> Bool 
checkCol col tickets rule = and $ map (\y -> check y rule) (getFromCol col tickets)

getColNum :: Rule -> [[Int]] -> Int -> Int
getColNum rule tickets max = fromJust $ elemIndex True $ map f [0..max-1]
    where f = (\y -> checkCol y tickets rule)

getColNums :: Rule -> [[Int]] -> Int -> [Int]
getColNums rule tickets max = filter f [0..max-1]
    where f = (\y -> checkCol y tickets rule)

main :: IO()
main = do
    file <- openFile "16-input.txt" ReadMode
    -- file <- openFile "16-ex.txt" ReadMode
    -- file <- openFile "16-ex2.txt" ReadMode
    cont <- hGetContents file
    let textraw = lines cont
        [rawrules, rawmy, rawnearby] = split "" textraw
        f = map (map (read :: String -> Int))
        nears = f $ map (split ',') $ tail rawnearby
        my = map (read :: String -> Int) $ split ',' $ last rawmy
        rules = map (getRule) rawrules
        valids = my : (filter (\y -> isValidT y rules) nears)
        totalcol = length $ head nears
        newrules = take 6 rules
        -- newrules = rules
        -- [r1,r2,r3,r4,r5,r6] = newrules
        thecols = map (\y -> getColNums y valids totalcol) newrules
        -- [r1,r2,r3] = rules
        -- wrongs = map (\y -> getWrong y rules) nears
        -- valids = map (\y -> isValid y rules) nears
    print nears
    mapM_ print valids
    print "here"
    mapM_ print thecols
    print my
