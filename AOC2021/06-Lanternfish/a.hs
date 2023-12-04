import System.IO

parseInput :: String -> [Int]
parseInput lin = map read $ words $ map commaToSpace $ lin
    where commaToSpace :: Char -> Char
          commaToSpace ',' = ' '
          commaToSpace x   = x

iterDay :: [Int] -> [Int]
iterDay [] = []
iterDay (x:xs)
    | x == 0    =   6:8: iterDay xs
    | otherwise = (x-1): iterDay xs

simulate :: Int -> [Int] -> [Int]
simulate 0 arr = arr
simulate x arr = simulate (x-1) $ iterDay arr

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin  = lines cont
        init = parseInput $ lin!!0
        fend = simulate 80 init
    print $ length fend
