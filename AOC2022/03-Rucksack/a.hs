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

getPriorities :: String -> Int
getPriorities sack = sum prio
    where len   = div (length sack) 2
          (l,r) = splitAt len sack
          same  = unique $ filter (\x -> elem x r) l
          prio  = map convertPriority same

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
    print $ map getPriorities lin
    print $ sum $ map getPriorities lin
