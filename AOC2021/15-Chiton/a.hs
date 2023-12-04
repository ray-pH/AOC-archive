import System.IO
import Data.Char
import Data.List

-- memoize :: (Pos -> a) -> [[a]]
-- memoize f (r,c) = map (\x -> map (f x) [0..]) [0..]
--
-- memoizeSearch :: (Grid -> Pos -> Pos -> a) -> grid ->  -> [[a]]
-- memoizeSearch f 

type Grid = [[Int]]
type Pos = (Int,Int)

parseLine :: [Char] -> [Int]
parseLine = map (\c -> ord c - ord '0')

isInGrid :: Pos -> Pos -> Bool
--isIngrid target pos
isInGrid (rt,ct) (r,c) = 0 <= r && r <= rt && 0 <= c && c <= ct

getVal :: Grid -> Pos -> Int
getVal grid (row,col) = (grid !! row) !! col

search :: Grid -> Pos -> Pos -> (Int,[Pos])
search grid target pos
    | pos == target = (val,[pos])
    | otherwise     = (count+val,pos:hist)
    where (row,col) = pos
          val = getVal grid pos
          -- toTraverse = filter (isInGrid target) [(row,col-1), (row,col+1), (row-1,col), (row+1,col)]
          toTraverse = filter (isInGrid target) [(row,col+1), (row+1,col)]
          results  = map (search grid target) toTraverse :: [(Int,[Pos])]
          smallest = head $ sort results :: (Int,[Pos])
          (count,hist) = smallest

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grid = map parseLine lin
        nrow = length grid
        ncol = length (grid !! 0)
        coords = [(r,c) | r<-[0..(nrow-1)], c<-[0..(ncol-1)]]
        initval = getVal grid (0,0)
        target = last coords
        path = search grid target (0,0)
        (v,_) = path
        ans = v - initval
    -- print coords
    -- print grid
    print path
    print ans
