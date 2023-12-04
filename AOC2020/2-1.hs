import System.IO 

front :: String -> String
front (x:xs)
    | head xs == '-' = [x]
    | otherwise = x : (front xs)

back :: String -> String
back (x:xs)
    | x == '-' = xs
    | otherwise = back xs

bound :: String -> [Integer]
bound st = [min, max]
    where
        str = (words st)!!0
        min = read (front str) :: Integer
        max = read (back str) :: Integer

char :: String -> Char
char st = (words st)!!1!!0

pass :: String -> String
pass st = (words st)!!2

count :: Eq a => a -> [a] -> Integer
count x = toInteger . length . filter (x==) 

isValid :: String -> Bool
isValid str = (min <= tcount) && (tcount <= max)
    where
        tpass = pass str
        tchar = char str
        tcount = count tchar tpass
        [min,max] = bound str

main :: IO()
main = do
    file <- openFile "2-input.txt" ReadMode
    contents <- hGetContents file
    let valid = filter isValid (lines contents)
    print (length valid)
