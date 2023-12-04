import System.IO

type BinStr = String

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

binArrToDec :: [Int] -> Int
binArrToDec [] = 0
binArrToDec (x:xs) = x*(2^pow) + binArrToDec xs
    where pow = length xs

addArr :: [Int] -> [Int] -> [Int]
addArr [] [] = []
addArr (x:xs) (y:ys) = (x+y): addArr xs ys

oxygenFind :: Int -> [[Int]] -> [Int]
oxygenFind _ (x:[])  = x
oxygenFind id arr = oxygenFind (id+1) filtered
    where init = take (length $ arr!!0) [0,0..]
          count = foldr addArr init arr
          common = map (\x -> 2*x >= length arr) count
          cid = boolToInt $ common!!id
          filtered = filter (\x -> (x!!id) == cid) arr

carbonFind :: Int -> [[Int]] -> [Int]
carbonFind _ (x:[])  = x
carbonFind id arr = carbonFind (id+1) filtered
    where init = take (length $ arr!!0) [0,0..]
          count = foldr addArr init arr
          uncommon = map (\x -> 2*x < length arr) count
          cid = boolToInt $ uncommon!!id
          filtered = filter (\x -> (x!!id) == cid) arr

main :: IO()
main = do
    file <- openFile "input.txt" ReadMode
    -- file <- openFile "inpex.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        arr = map (map charToInt) lin
        oxygen = binArrToDec $ oxygenFind 0 arr
        carbon = binArrToDec $ carbonFind 0 arr
    print $ carbon*oxygen
