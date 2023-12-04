module Main where

import System.IO

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = toBin (quot n 2) ++ [rem n 2]

leadingBin :: [Int] -> [Int]
leadingBin bin 
    | length bin == 36 = bin
    | otherwise = leadingBin $ 0:bin

toDecimal :: [Int] -> Int
toDecimal arr = sum [ 2^n * x | (n,x) <- zip [0..] (reverse arr) ]

-- mem[8] = 11
toOp :: String -> (Int,Int)
toOp s = (address, val)
    where [a,_,b] = words s
          val = read b
          ('m':'e':'m':'[':xs) = a
          address = read $ init xs

isMask :: String -> Bool
isMask s = if take 4 s == "mask" then True else False

isInMem :: [(Int,Integer)] -> Int -> Bool
isInMem mem address = elem address [x | (x,_)<-mem]

replaceMem :: [(Int,Integer)] -> Int -> Integer -> [(Int,Integer)]
replaceMem mem address val = [ (x,if x == address then val else y) | (x,y) <- mem ]

dig :: Char -> Int
dig '0' = 0
dig '1' = 1

masking :: [Int] -> String -> [Int]
masking val mask = [ if m=='X' then x else (dig m) | (x,m) <- zip val mask ]

calcVal :: String -> Int -> Int
calcVal mask val = let bin = leadingBin $ toBin val
                       masked = masking bin mask
                    in toDecimal masked

-- calcAll
runProc :: [(Int,Integer)] -> [String] -> String -> [(Int,Integer)]
runProc mem [] _ = mem
runProc mem (instr:instrs) mask
    | isMask instr = runProc mem instrs (last $ words instr)
    | otherwise = let (address,val) = toOp instr
                      val' = toInteger $ calcVal mask val
                      mem' = if isInMem mem address
                                then replaceMem mem address val'
                                else mem ++ [(address,val')]
                   in runProc mem' instrs mask 
    

main :: IO()
main = do
    file <- openFile "14-input.txt" ReadMode
    -- file <- openFile "14-ex.txt" ReadMode
    cont <- hGetContents file
    let l = lines cont
        mem = []
        mem' = runProc mem l (replicate 36 'X')
        res = sum [ val | (_,val) <- mem' ]
    print mem'
    print res
