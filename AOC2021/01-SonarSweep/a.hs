import System.IO

accHigher :: [Integer] -> Integer
accHigher [_] = 0
accHigher (x:y:xs)
    | (y>x) = 1 + accHigher (y:xs)
    | otherwise = accHigher (y:xs)

main :: IO()
main = do
    handle   <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lis = map read (words contents) :: [Integer]
    print $ accHigher lis
