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

toExplore :: [Explorer] -> Explorer -> Bool
toExplore history (val,pos)
    | 0 == length pexps = True
    | otherwise = val < pval
    where pexps = filter (\(v,p) -> p == pos) history
          (pval,_) = head pexps

explore :: Grid -> [Explorer] -> Explorer -> [Explorer]
explore grid history (val,p) = newexp
    where dirs = genDirs grid p
          zipval = map (\p -> (val + (getVal grid p), p)) dirs :: [Explorer]
          newexp = filter (toExplore history) zipval :: [Explorer]

appendHist :: Explorer -> [Explorer] -> [Explorer]
appendHist (val,pos) hist 
    | isExist   = map (fappend (val,pos)) hist
    | otherwise = (val,pos) : hist
    where isExist = 0 < (length $ filter (\(v,p) -> p == pos) hist)
          fappend :: Explorer -> Explorer -> Explorer
          fappend (v1,p1) (v2,p2)
              | p1 == p2  = (v1,p1)
              | otherwise = (v2,p2)

exploreAll :: Grid -> [Explorer] -> [Explorer] -> [Explorer]
exploreAll grid history [] = history
exploreAll grid history exps = exploreAll grid newhist (es++newexps)
    where (e:es) = exps
          newexps = explore grid history e :: [Explorer] --new explorer
          -- newexps = trace (show history ++ " " ++ show newexps) explore grid history e :: [Explorer] --new explorer
          newhist = trace (show history ++ " : " ++ show newexps ++ " <- " ++ show e) appendHist e history
          -- newhist = appendHist e history
          -- newhist = foldr appendHist history newexps

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grid = map parseLine lin
        nrow = length grid
        ncol = length (grid !! 0)
        target = (nrow-1,ncol-1)
        initpos = (0,0)
        explored = exploreAll grid [] [(0,initpos)]
        (res,_)  = head $ filter (\(v,p) -> p == target) explored
    -- print grid
    print explored
    -- print res
