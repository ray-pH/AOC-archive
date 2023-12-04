import System.IO 

combine :: [String] -> [String]
combine [] = []
combine (x:[]) = [x]
combine (x:xs) 
    | head xs == "" = x : (combine $ tail xs)
    | otherwise = combine ((x ++ (head xs)) : (tail xs))

count :: String -> Int
count s = length $ filter (\y -> elem y s) ['a'..'z']

main :: IO()
main = do
    file <- openFile "6-input.txt" ReadMode
    cont <- hGetContents file
    let arr = lines cont
    let combined = combine arr
    let howm = map count combined
    print $ sum howm
    -- mapM_ print combined
    -- print "saf"
