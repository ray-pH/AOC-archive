import System.IO

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

isUnique :: Eq a => [a] -> Bool
isUnique xs = xs == unique xs

genFourWide :: Eq a => Int -> [a] -> [a]
genFourWide startid = take 14 . drop startid

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpex2.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont !! 0
        len = length lin
        arr = [1..(len-14)]
        fwides = map (\x -> genFourWide x lin) arr
        zipped = zip [1..] fwides
        uniq = filter (\(id,val) -> isUnique val) zipped
        (id,val) = head uniq
    -- print lin
    -- print fwides
    print $ head uniq
    print $ id + 14
