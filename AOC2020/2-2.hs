import System.IO 

front :: String -> String
front (x:xs)
    | head xs == '-' = [x]
    | otherwise = x : (front xs)

back :: String -> String
back (x:xs)
    | x == '-' = xs
    | otherwise = back xs

bound :: String -> [Int]
bound st = [min, max]
    where
        str = (words st)!!0
        min = read (front str) :: Int
        max = read (back str) :: Int

char :: String -> Char
char st = (words st)!!1!!0

pass :: String -> String
pass st = (words st)!!2

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==) 

isValid :: String -> Bool
isValid str = (count c res) == 1
    where
        c = char str
        tpass = pass str
        [a,b] = bound str
        res = [tpass!!(a-1), tpass!!(b-1)]

main :: IO()
main = do
    file <- openFile "2-input.txt" ReadMode
    contents <- hGetContents file
    let valid = filter isValid (lines contents)
    print (length valid)
