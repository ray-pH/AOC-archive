module Main where

import System.IO

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

-- isValid :: [Int] -> [Rule] -> Bool
-- isValid ticket rules = and [ or [ check i r | r <- rules  ] | i <- ticket ]

customHead :: [Int] -> Int
customHead [] = 0
customHead s = head s

getWrong :: [Int] -> [Rule] -> Int
getWrong ticket rules = customHead $ filter (\y -> not $ checkAllRule y rules) ticket
                    

main :: IO()
main = do
    file <- openFile "16-input.txt" ReadMode
    -- file <- openFile "16-ex.txt" ReadMode
    cont <- hGetContents file
    let textraw = lines cont
        [rawrules, rawmy, rawnearby] = split "" textraw
        f = map (map (read :: String -> Int))
        nears = f $ map (split ',') $ tail rawnearby
        rules = map (getRule) rawrules
        wrongs = map (\y -> getWrong y rules) nears
        -- valids = map (\y -> isValid y rules) nears
    -- print $ isValid [7,3,47] rules
    print nears 
    print wrongs
    print $ sum wrongs
    -- mapM_ print rules
