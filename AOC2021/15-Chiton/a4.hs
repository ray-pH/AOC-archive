import System.IO
import Data.Char
import Data.List
import Debug.Trace

type Grid = [[Int]]
type Pos = (Int,Int)
type Explorer = (Int,Pos)

parseLine :: [Char] -> [Int]
parseLine = map (\c -> ord c - ord '0')

isInGrid :: Grid -> Pos -> Bool
isInGrid grid (r,c) = 0 <= r && r < nrow && 0 <= c && c < ncol
    where nrow = length grid
          ncol = length (grid !! 0)

genDirs :: Grid -> Pos -> [Pos]
genDirs grid (r,c) = filter (isInGrid grid) [(r,c+1), (r+1,c)]

getVal :: Grid -> Pos -> Int
getVal grid (row,col) = (grid !! row) !! col

getValHist :: [Explorer] -> Pos -> Int
getValHist [] _ = 0
getValHist ((v,p):rs) pos 
  | p == pos = v
  | otherwise = getValHist rs pos

doExpand :: Grid -> Pos -> [Explorer] -> Bool
doExpand grid pos hist
    | 
    where val  = getVal grid pos
          nrow = length grid
          ncol = length (grid !! 0)
          target = (nrow-1,ncol-1)



main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grid = map parseLine lin
        nrow = length grid
        ncol = length (grid !! 0)
        target = (nrow-1,ncol-1)
        initpos = (0,0)
    -- print grid
    print $ explore grid target [(0,initpos)]
