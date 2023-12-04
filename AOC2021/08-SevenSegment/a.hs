import System.IO

  -- 0:      1:      2:      3:      4:
 -- aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
 -- ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
 -- gggg    ....    gggg    gggg    ....

  -- 5:      6:      7:      8:      9:
 -- aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
 -- dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
 -- gggg    gggg    ....    gggg    gggg

type Entry = ([String],[String])

parseLine :: String -> Entry
parseLine x = (inp x, out x)
    where inp = takeWhile ((/=) "|") . words
          out = tail . dropWhile ((/=) "|") . words

uniqueSegment :: String -> Bool
uniqueSegment st = elem (length st) [2, 3, 4, 7]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        entries = map parseLine lin
        outputs = map (\(x,y) -> y) entries
        flatout = flatten outputs
        res = length $ filter uniqueSegment flatout
    print res
