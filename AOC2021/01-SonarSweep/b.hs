import System.IO

combineThree :: [Integer] -> [Integer]
combineThree (x:y:z:xs) = (x+y+z): combineThree (y:z:xs)
combineThree _ = []

accHigher :: [Integer] -> Integer
accHigher [x] = 0
accHigher (x:y:xs)
    | (y>x) = 1 + accHigher (y:xs)
    | otherwise = accHigher (y:xs)

main :: IO()
main = do
    handle   <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lis = map read (words contents) :: [Integer]
    print $ accHigher $ combineThree lis
