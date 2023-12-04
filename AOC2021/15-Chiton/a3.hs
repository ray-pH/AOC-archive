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

flip :: (a,b) -> (b,a)
flip (x,y) = (y,x)

-- concat :: [Explorer] -> [Explorer]
-- concat [] = []
-- concat [x] = [x]
-- concat (a:b:xs) = 
--

combineExp :: Explorer -> [Explorer] -> [Explorer]
combineExp (val,pos) [] = [(val,pos)]
combineExp (val,pos) (e:es)
    | p == pos  = (min val v, pos):es
    | otherwise = e : combineExp (val,pos) es
    where (v,p) = e

explore :: Grid -> Pos -> [Explorer] -> Explorer
explore grid target (e:es)
    | pos == target = e
    | otherwise     = explore grid target newsorted
    where (val,pos) = trace (show e) e
    -- where (val,pos) = e
          dirs   = genDirs grid pos
          vals   = map (+ val) $ map (getVal grid) dirs
          newexp = zip vals dirs
          newapp = newexp ++ es
          newcon = foldr combineExp es newapp
          newsorted = sort newcon

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
