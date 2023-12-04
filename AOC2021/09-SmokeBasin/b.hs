import System.IO
import Data.Char
import Data.List

type Pos  = (Int,Int)
type Board = [[Int]]

parseInp :: [String] -> Board
parseInp = map (map (\c -> ord c - ord '0' ))

padBoard :: Board -> Board
padBoard board = rowpad : padlr board ++ [rowpad]
    where padlr = map (\row -> [9] ++ row ++ [9])
          len   = 2 + length (board!!0)
          rowpad = take len $ repeat 9

getVal :: Board -> Pos -> Int
getVal board (row,col) = board !! row !! col

getNeighCoord :: Pos -> [Pos]
getNeighCoord (row,col) = [n1,n2,n3,n4]
    where n1 = (row+1,col)
          n2 = (row-1,col)
          n3 = (row,col+1)
          n4 = (row,col-1)

getNeigh ::  Board -> Pos -> [Int]
getNeigh board pos = map (getVal board) $ getNeighCoord pos

isLowest :: Board -> Pos -> Bool
isLowest board pos = and $ map (\n -> n > val) neigh
    where val   = getVal board pos
          neigh = getNeigh board pos

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) 
    | elem x xs = unique xs
    | otherwise = x : unique xs

genCoords :: Int -> Int -> [Pos]
genCoords nrow ncol = flatten $ map (\(a,b) -> zip a b) $ zip (map (take ncol . repeat) [1..nrow]) (repeat [1..ncol])

getLower :: Board -> Pos -> [Pos]
getLower board pos
    | isLowest board pos = [pos]
    | otherwise = filter (\p -> val > getVal board p) neigh
    where val = getVal board pos
          neigh = getNeighCoord pos

getTargets :: Board -> [Pos] -> [Pos]
getTargets board posses
    | posses == newposses = posses
    | otherwise = getTargets board newposses
    where newposses = unique $ flatten $ map (getLower board) posses

getBasins :: Board -> [Pos] -> [[Pos]]
getBasins _ [] = []
getBasins board (c:cs)
    | val == 9  = [] : next
    | otherwise = targets : next
    where val   = getVal board c
          next  = getBasins board cs
          targets = getTargets board [c]

filterBasins :: [[Pos]] -> [Pos]
filterBasins = flatten . filter (\arr -> 1 == length arr)

countSeq :: Eq a => [a] -> Int -> [Int]
countSeq [] _  = []
countSeq (x:[]) acc = [acc+1]
countSeq (x:y:rs) acc
  | x == y = countSeq (y:rs) (acc+1)
  | otherwise = (acc+1) : countSeq (y:rs) 0


main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        board  = parseInp lin
        nrow   = length board
        ncol   = length $ head board
        pboard = padBoard board
        coords = genCoords nrow ncol
        basincoords = sort $ filterBasins $ getBasins pboard coords
        basinsizes  = countSeq basincoords 0
    print $ product $ take 3 $ reverse $ sort basinsizes
