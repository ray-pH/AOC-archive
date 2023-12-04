module Main where

import System.IO

data Rule = Rule { rid :: Int, refs :: [[Int]], val :: Char }
        deriving Show

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- parseraw accept "2 1 | 2 3"
parseraw :: String -> [[Int]]
parseraw st = case elem '|' st of
                False -> [map read $ words st]
                True -> let [rleft,rright] = split '|' st
                            left = map read $ words $ init rleft
                            right = map read $ words $ tail rright
                         in [left,right]

getRule :: String -> Rule
getRule st = let [id, rawr] = split ':' st
              in case elem '"' rawr of
                   True  -> Rule (read id) [] (last $ init rawr)
                   False -> Rule (read id) (parseraw $ tail rawr) 'x'

getRuleById :: [Rule] -> Int -> Rule
getRuleById rules theid = head $ filter (\y-> theid == rid y ) rules

-- isFollowingRule :: String -> Rule -> [Rule] -> Bool
-- isFollowingRule (s:ss) rule rls = case (val rule == 'x') of
--         False -> (s == val rule)
--         True  -> or $ map f $ refs rule
--             where rules' = map (\y -> getRuleById y rls)
--                   f :: [Int] -> Boo
--                   f ints = isFollowingRule 

safeGenRegex :: [Rule] -> Rule -> String
-- safeGenRegex = genRegex
safeGenRegex rs r
    | (rid r) == 8 = "(" ++ (safeGenRegex rs $ f 42) ++ ")+"
    | (rid r) == 11 = "(" ++ (safeGenRegex rs $ f 42) ++ ")+(" ++ (safeGenRegex rs $ f 31) ++ ")+"
    | otherwise = genRegex rs r
    where f n = getRuleById rs n

genRegex :: [Rule] -> Rule -> String
genRegex rs r
    | (length $ refs r) == 0 = (val r):""
    | (length $ refs r) == 1 = let rules = map (getRuleById rs) (head $ refs r)
                                in foldr (++) "" $ map (safeGenRegex rs) rules
    | otherwise = let [rules1,rules2] = map (map (getRuleById rs)) $ refs r -- [[Rule]]
                      res1 = foldr (++) "" $ map (safeGenRegex rs) rules1 -- String
                      res2 = foldr (++) "" $ map (safeGenRegex rs) rules2 -- String
                   in "(" ++ res1 ++ "|" ++ res2 ++ ")"

--- fuck this shit i'm using regex
--
main :: IO()
main = do
    file <- openFile "19-input.txt" ReadMode
    -- file <- openFile "19-ex2.txt" ReadMode
    cont <- hGetContents file
    let l = lines cont
        [strrules,strmsgs] = split "" l
        rules = map getRule strrules
        r0 = getRuleById rules 0
        reg = safeGenRegex rules r0
    -- mapM_ print strrules
    -- mapM_ print strmsgs
    print $ length rules
    print r0
    print reg
