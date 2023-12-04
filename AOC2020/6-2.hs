import System.IO 

combine :: [String] -> [String]
combine [] = []
combine (x:[]) = [x]
combine (x:xs) 
    | head xs == "" = x : (combine $ tail xs)
    | otherwise = combine ((x ++ " " ++ (head xs)) : (tail xs))

existinall :: Char -> [String] -> Bool
existinall c st = and $ map (\y -> elem c y) st

count :: String -> Int
count s = length $ filter (\y -> existinall y st) ['a'..'z']
    where st = words s

main :: IO()
main = do
    file <- openFile "6-input.txt" ReadMode
    cont <- hGetContents file
    let arr = lines cont
    let groups = combine arr
    let howm = map count groups
    print $ sum howm
    -- mapM_ print groups
    -- print "saf"
