import System.IO

parseInput :: String -> [Int]
parseInput = map (read :: String -> Int) . words . map commaToSpace
    where commaToSpace :: Char -> Char
          commaToSpace ',' = ' '
          commaToSpace x   = x

naturalsum :: Int -> Int
naturalsum x = div (x*(x+1)) 2

fuelCost :: Int -> [Int] -> Int
fuelCost destination = sum . map naturalsum . map distance
    where distance = (\a -> abs $ a - destination)

mini :: [Int] -> Int
mini [x] = x
mini (x:y:xs)
    | x < y = mini $ x:xs
    | otherwise = mini $ y:xs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        inp = parseInput $ lin!!0
        space = [0..(foldr max 0 inp)]
        costs = map (\x -> fuelCost x inp) space
        res = mini costs
    -- print costs
    print res
