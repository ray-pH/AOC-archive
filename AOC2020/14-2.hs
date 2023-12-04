module Main where

import System.IO

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = toBin (quot n 2) ++ [rem n 2]

listId :: Eq a => a -> [a] -> [Int]
listId e arr = [ i | (i,a) <- zip [0..] arr, a == e ]

genBin :: Int -> [String]
genBin n = [ fixLength n $ binToStr $ toBin i | i<-[0..2^n-1] ]

fixLength :: Int -> String -> String
fixLength n st
    | length st == n = st
    | length st > n = fixLength n (tail st)
    | otherwise = fixLength n ('0':st)

leadingBin :: [Int] -> [Int]
leadingBin bin 
    | length bin == 36 = bin
    | otherwise = leadingBin $ 0:bin

strLeadingBin :: String -> String
strLeadingBin bin 
    | length bin == 36 = bin
    | otherwise = strLeadingBin $ '0':bin

toDecimal :: [Int] -> Int
toDecimal arr = sum [ 2^n * x | (n,x) <- zip [0..] (reverse arr) ]

binToStr :: [Int] -> String
binToStr arr = map (\y -> if y == 1 then '1' else '0' ) arr

strToBin :: String -> [Int]
strToBin str = map (\y -> if y == '1' then 1 else 0 ) str

-- mem[8] = 11
toOp :: String -> (Int,Int)
toOp s = (address, val)
    where [a,_,b] = words s
          val = read b
          ('m':'e':'m':'[':xs) = a
          address = read $ init xs

isMask :: String -> Bool
isMask s = if take 4 s == "mask" then True else False

isInMem :: [(Int,Int)] -> Int -> Bool
isInMem mem address = elem address [x | (x,_)<-mem]

replaceMem :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
replaceMem mem address val = [ (x,if x == address then val else y) | (x,y) <- mem ]

masking :: String -> String -> String
masking binst mask = [ if m=='0' then x else m | (x,m) <- zip binst mask ]

replace :: Eq a => Int -> a -> [a] -> [a]
replace n e arr = [ if i == n then e else c | (c,i) <- zip arr [0..] ]

replaceArr :: String -> [(Int,Char)] -> String
replaceArr st [] = st
replaceArr st (x:xs) = replaceArr (replace a b st) xs
    where (a,b) = x
-- [2,5,7] "101"
-- (2,'1'),(5,'0'),(7,'1')
applyFloat :: String -> String -> String
applyFloat str bin = let ids = listId 'X' str
                         zipped = zip ids bin
                      in replaceArr str zipped

genAllVal :: String -> [Int]
genAllVal res = let bins = genBin (length $ filter (== 'X') res) 
                    strs = [ applyFloat res b | b <- bins ]
                    valbin = map strToBin strs
                 in map toDecimal valbin

-- calcVal :: String -> Int -> Int
-- calcVal mask val = let bin = leadingBin $ toBin val
--                        masked = masking bin mask
--                     in toDecimal masked

memArr :: [(Int,Int)] -> [Int] -> Int -> [(Int,Int)]
memArr mem [] _ = mem
memArr mem (address:adds) val = memArr mem' adds val
            where mem' = if isInMem mem address 
                            then replaceMem mem address val
                            else mem ++ [(address,val)]
--
-- calcAll
runProc :: [(Int,Int)] -> [String] -> String -> [(Int,Int)]
runProc mem [] _ = mem
runProc mem (instr:instrs) mask
    | isMask instr = runProc mem instrs (last $ words instr)
    | otherwise = let (address,val) = toOp instr
                      f = strLeadingBin . binToStr . toBin
                      ads = genAllVal $ masking (f address) mask
                      mem' = memArr mem ads val
                   in runProc mem' instrs mask

main :: IO()
main = do
    file <- openFile "14-input.txt" ReadMode
    -- file <- openFile "14-ex2.txt" ReadMode
    cont <- hGetContents file
    let l = take 20 $ lines cont
        mem = []
        mem' = runProc mem l (replicate 36 'X')
        res = sum [ toInteger val | (_,val) <- mem' ]
    print l
    print "helyeah"
    -- print mem'
    print res
