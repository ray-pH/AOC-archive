import System.IO

type BinStr = String
type BinArr = [Bool]

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1

binArrToDec :: BinArr -> Int
binArrToDec [] = 0
binArrToDec (x:xs)
    | x     = (2^pow) + binArrToDec xs
    | not x = binArrToDec xs
    where pow = length xs

addArr :: [Int] -> [Int] -> [Int]
addArr [] [] = []
addArr (x:xs) (y:ys) = (x+y): addArr xs ys

main :: IO()
main = do
    file <- openFile "input.txt" ReadMode
    -- file <- openFile "inpex.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        arr = map (map charToInt) lin
        init = take (length $ arr!!0) [0,0..]
        count = foldr addArr init arr
        binarr  = map (\x -> 2*x >= length arr) count
        gamma   = binArrToDec binarr
        epsilon = binArrToDec $ map not binarr
    print $ gamma * epsilon
