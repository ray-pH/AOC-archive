import System.IO

combine :: [String] -> [String]
combine [] = []
combine (x:[]) = [x]
combine (x:xs) 
    | head xs == "" = x : (combine $ tail xs)
    | otherwise = combine ((x ++ " " ++ (head xs)) : (tail xs))

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

isAllIn :: String -> String -> Bool
isAllIn st guide = and [elem c guide | c <- st]

front :: String -> String
front (x:xs)
    | head xs == ':' = [x]
    | otherwise = x : (front xs)

back :: String -> String
back (x:xs)
    | x == ':' = xs
    | otherwise = back xs

byr :: String -> Bool
byr s = let i = read s :: Int
        in 1920 <= i && i <= 2002

iyr :: String -> Bool
iyr s = let i = read s :: Int
        in 2010 <= i && i <= 2020

eyr :: String -> Bool
eyr s = let i = read s :: Int
        in 2020 <= i && i <= 2030

pid :: String -> Bool
pid s = (isAllIn s "0123456789") && (length s == 9)

ecl :: String -> Bool
ecl s = elem s ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hcl :: String -> Bool
hcl s = let f = head s
            b = tail s
         in (f == '#') && (isAllIn b "0123456789abcdef")

inhgt :: String -> Bool
inhgt s 
    | ln < 2 = False
    | otherwise = substring (ln-2) ln s == "in" 
    where ln = length s

cmhgt :: String -> Bool
cmhgt s 
    | ln < 2 = False
    | otherwise = substring (ln-2) ln s == "cm" 
    where ln = length s

hgt :: String -> Bool
hgt s
  | cmhgt s = 150 <= i && i <= 193
  | inhgt s = 59 <= i && i <= 76
  | otherwise = False
  where i = read (substring 0 (ln -2 ) s) :: Int
        ln = length s

isComplete :: String -> Bool
isComplete st = 
    let guide = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        wordlist = words st
        keys = map front wordlist
        bools = [elem g keys | g <- guide]
     in and bools

isValid :: String -> Bool
isValid st 
    | isComplete st = 
    let guide = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        wordlist = words st
        p = [ y | x<-guide, y<-wordlist, (front y) == x ]
        funcs = [byr, iyr, eyr, hgt, hcl, ecl, pid]
        bools = [ f (back i) | (f,i) <- zip funcs p ]
    in and bools
    | otherwise = False


main :: IO()
main = do
     -- file <- openFile "4-ex-val.txt" ReadMode
     file <- openFile "4-input.txt" ReadMode
     cont <- hGetContents file
     let arr = lines cont
     let combined = combine arr
     let valids = [p | p<-combined, isValid p]
     mapM_ print valids
     print $ length valids
