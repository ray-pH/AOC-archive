import System.IO
import Data.List

type Column = (Int, String)
type Board = [Column]

dropR n = reverse . drop n . reverse
takeR n = reverse . take n . reverse

updateBoard :: Column -> Board -> Board
updateBoard (id,str) board = left ++ [(id,str)] ++ right
    where left  = take (id-1) board
          right = drop id board

moveN :: Int -> Int -> Int -> Board -> Board
moveN frid toid n board = updateBoard (frid,frstr') $ updateBoard (toid, tostr') board
    where (_, frstr) = board !! (frid - 1)
          (_, tostr) = board !! (toid - 1)
          picked  = takeR n $ frstr :: String
          frstr'  = dropR n $ frstr :: String
          tostr'  = tostr ++ picked

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

parseColumn :: String -> Column
parseColumn (x:xs) = (read (x:""), filter (\r -> r /= ' ') xs)

genBoard :: [String] -> Board
genBoard x = map parseColumn x

collectColumn :: [String] -> [String]
collectColumn strs = map (\x -> reverse (x !! 1)) grouped
    where grouped = splitBy (expandchar ' ' $ length (strs!!0)) strs
          expandchar :: Char -> Int -> [Char]
          expandchar _ 0 = ""
          expandchar c n = c : expandchar c (n-1)

-- ["move","1","from","2","to","1"]
parseOperations :: String -> (Int,Int,Int)
parseOperations str = (from, to, n)
    where w = words str
          n    = read $ w !! 1
          from = read $ w !! 3
          to   = read $ w !! 5

apply :: [(Int,Int,Int)] -> Board -> Board
apply [] board = board
apply ((from,to,n):ops) board = apply ops $ moveN from to n board

getTop :: Board -> String
getTop = map (\(id,str) -> last str)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        [stacks, operations] = splitBy "" lin
        transposed = transpose stacks
        columns = collectColumn transposed
        board   = genBoard columns
        ops     = map parseOperations operations
    -- mapM_ print transposed
    print board
    print $ apply ops board
    print $ getTop $ apply ops board
    -- print $ moveOnce 2 1 board
    -- print operations
    -- print ops
    -- print $ parseOperations (operations !! 0)
