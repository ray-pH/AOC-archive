import System.IO

combine :: [String] -> [String]
combine [] = []
combine (x:[]) = [x]
combine (x:xs) 
    | head xs == "" = x : (combine $ tail xs)
    | otherwise = combine ((x ++ " " ++ (head xs)) : (tail xs))


front :: String -> String
front (x:xs)
    | head xs == ':' = [x]
    | otherwise = x : (front xs)

isValid :: String -> Bool
isValid st =
    let guide = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        wordlist = words st
        keys = map front wordlist
        bools = [elem g keys | g <- guide]
     in and bools

main :: IO()
main = do
     file <- openFile "4-input.txt" ReadMode
     cont <- hGetContents file
     let arr = lines cont
     let combined = combine arr
     let valids = [p | p<-combined, isValid p]
     -- mapM_ print valids
     print $ length valids
