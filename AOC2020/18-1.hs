module Main where

import System.IO
import Data.List
 
containOperand :: [String] -> Bool
containOperand lis = (elem "+" lis) || (elem "*" lis)

fixBracket :: String  -> [String]
fixBracket "" = []
fixBracket ('(':xs) = "(" : fixBracket xs
fixBracket s 
    | last s == ')' = (fixBracket $ init s) ++ [")"]
    | otherwise = [s]

fixListBracket :: [String] -> [String]
fixListBracket [] = []
fixListBracket (x:xs) = (fixBracket x) ++ fixListBracket xs

evalUnit :: [String] -> [String]
evalUnit (x:[]) = [x]
evalUnit (x:c:y:xs) = evalUnit $ (show evaluated) : xs
    where intx = read x
          inty = read y
          evaluated = (if c=="+" then (+) else (*)) intx inty

getPair :: Int -> Int -> [String] -> Int
getPair 0 index (")":_) = index
getPair acc index ("(":xs) = getPair (acc+1) (index+1) xs
getPair acc index (")":xs) = getPair (acc-1) (index+1) xs
getPair acc index (_:xs) = getPair acc (index+1) xs

subset :: Eq a => Int -> Int -> [a] -> [a]
subset start end arr = take (end - start) (drop start arr)

eval :: [String] -> [String]
eval sts = case elemIndex "(" sts of 
            Nothing -> evalUnit sts
            Just val -> let lastid = getPair 0 (val+1) $ tail $ drop val sts
                            left  = take val sts
                            right = drop (lastid+1) sts
                            mid   = subset (val+1) lastid sts
                            mid' = eval mid
                         in eval $ left ++ mid' ++ right 



main :: IO()
main = do
    file <- openFile "18-input.txt" ReadMode
    -- file <- openFile "18-ex.txt" ReadMode
    cont <- hGetContents file
    let inp = lines cont
        winp = map words inp
        finp = map fixListBracket winp
        evaluated = map eval finp
        res = map (read :: String -> Int) $ map head evaluated
    mapM_ print finp
    print "eval"
    mapM_ print res
    print "sum"
    print $ sum res
    -- print $ eval $ words inp
    -- mapM_ print l
        
