import System.IO
import Data.Char
import Data.List

type Grid = [[Int]]
type Pos = (Int,Int)
type Explorer = (Pos,Int) -- curr, val

parseLine :: [Char] -> [Int]
parseLine = map (\c -> ord c - ord '0')

getVal :: Grid -> Pos -> Int
getVal grid (row,col) = (grid !! row) !! col

isExplored :: Pos -> [(Pos,Int)] -> Bool
isExplored pos = elem pos . map (\(p,x) -> p)

isInGrid :: Grid -> Pos -> Bool
isInGrid grid (r,c) = 0 <= r && r < nrow && 0 <= c && c < ncol
    where nrow = length grid
          ncol = length (grid !! 0)

genDirs :: Grid -> Pos -> [Pos]
genDirs grid (r,c) = filter (isInGrid grid) [(r,c+1), (r+1,c)]

isShorter :: [Explorer] -> Explorer -> Bool
isShorter [] _ = []
isShorter ((p,v):es) (pos,val) 
    | pos == p  = val < v
    | otherwise = isShorter exp es

explore :: Grid -> [Explorer] -> Pos -> [Explorer]
explore grid explored pos =
    where val   = getVal grid pos
          dirs  = genDirs grid pos
          unexp = filter (\p -> isExplored x explored) dirs
          unval = map (getVal grid) unexp
          unzip = zip (unexp, unval)
          unnew = map (\(p,v) -> (p,v+val)) unzip :: [Explorer]
              ---
          isexp = filter (\p -> not (isExplored x explored)) dirs
          isval = map (getVal grid) isexp
          iszip = zip (isexp, isval)
          isnew = filter (isShorter explored) $ map (\(p,v) -> (p,v+val)) iszip :: [Explorer]

fconcatExp :: [Explorer] -> [Explorer]
fconcatExp (_:[]) = []
fconcatExp ((pa,va):(pb,vb):xs)
    | pa == va && va 
concatExp :: [Explorer] -> [Explorer]
concatExp = fconcatExp . sort

search :: Grid -> [(Pos,Int)] -> [Explorer] -> Grid
search grid explored [] = vgrid
search grid explored exps =
    where 



main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grid = map parseLine lin :: Grid
        vgrid = map (map (\x -> 0)) grid :: Grid
        nrow = length grid
        ncol = length (grid !! 0)
        -- coords = [(r,c) | r<-[0..(nrow-1)], c<-[0..(ncol-1)]]
        initval = getVal grid (0,0)
    print grid
    print vgrid
