import System.IO
import Data.Char

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

convertPriority :: Char -> Int
convertPriority c
  | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
  | 'A' <= c && c <= 'Z' = (ord c) - (ord 'A') + 27

-- getPriorities :: String -> Int
-- getPriorities sack = sum prio
--     where len   = div (length sack) 2
--           (l,r) = splitAt len sack
--           same  = unique $ filter (\x -> elem x r) l
--           prio  = map convertPriority same
getBadge :: [String] -> Int
getBadge (a:b:c:[]) = sum $ map convertPriority abc
    where ab  = unique $ filter (\x -> elem x b) a
          abc = unique $ filter (\x -> elem x c) ab

group :: [String] -> [[String]]
group [] = []
group li = (take 3 li) : group (drop 3 li)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
    -- print $ group lin
    print $ map getBadge $ group lin
    print $ sum $ map getBadge $ group lin
    -- print $ map getPriorities lin
    -- print $ sum $ map getPriorities lin
