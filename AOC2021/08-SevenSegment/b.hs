import System.IO
import Data.List
import Data.Char
import Debug.Trace

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

allSeg  = "abcdefg"
allPerm = permutations allSeg
initGuess = take (length allSeg) $ repeat [1,1,1,1,1,1,1]
seg :: [[Bool]]
seg = [[True ,True ,True ,False,True ,True ,True ],
    [False,False,True ,False,False,True ,False],
    [True ,False,True ,True ,True ,False,True ],
    [True ,False,True ,True ,False,True ,True ],
    [False,True ,True ,True ,False,True ,False],
    [True ,True ,False,True ,False,True ,True ],
    [True ,True ,False,True ,True ,True ,True ],
    [True ,False,True ,False,False,True ,False],
    [True ,True ,True ,True ,True ,True ,True ],
    [True ,True ,True ,True ,False,True ,True ]]

toNum :: Char -> Int
toNum c = ord c - ord 'a'
toChar :: Int -> Char
toChar x = chr (x + ord 'a')

codeToInt :: [Bool] -> Int
codeToInt code = res
    where zipped = zip [0..] seg
          (res,_)  = head $ filter (\(id,s) -> s == code) zipped


parseLine :: String -> Entry
parseLine x = (inp x, out x)
    where inp = takeWhile ((/=) "|") . words
          out = tail . dropWhile ((/=) "|") . words

uniqueSegment :: String -> Bool
uniqueSegment st = elem (length st) [2, 3, 4, 7]

encode :: String -> [Bool]
encode cs = map (\c -> elem c cs) allSeg

decode :: [Bool] -> String
decode code = map (\(a,b) -> a) filtered
    where zipped   = zip allSeg code
          filtered = filter (\(c,bool) -> bool) zipped

permuteCase :: String -> String -> String
permuteCase _ "" = ""
permuteCase perm (c:cs) = newc : permuteCase perm cs
    where newc = perm !! (toNum c)

testPerm :: [String] -> String -> Bool
testPerm cases perm = and logical
    where permutted = map (\s -> permuteCase perm s) cases
          encoded   = map encode permutted
          logical   = map (\e -> elem e seg) encoded

guessPerm :: Entry -> String
guessPerm (inp,out) = res
    where arr     = sort $ inp ++ out
          guesses = zip allPerm $ map (\p -> testPerm arr p) allPerm
          (res,_) = head $ filter (\(p,bool) -> bool) guesses

getValue :: Entry -> Int
getValue entry = read resStr
    where perm    = guessPerm entry
          (_,out) = entry
          permutted = map (\s -> permuteCase perm s) out
          encoded   = map encode permutted
          vals      = map codeToInt encoded
          resStr    = map (\x -> chr(x + ord('0'))) vals

main :: IO()
main = do
    -- file <- openFile "inppp.txt" ReadMode
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        entries = map parseLine lin
    print $ map getValue entries
    print $ sum $ map getValue entries
