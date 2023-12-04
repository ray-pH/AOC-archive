import System.IO
import Data.Char
import Data.List

type Pos  = (Int,Int)
type Cell = (Pos,Int)
type Board = [Cell]
boardSize = 10 :: Int

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

parseInp :: [String] -> Board
parseInp = toBoard . flatten . bringrow . ziprow . zipcol . map readChar
    where readChar = map (\c -> ord c - ord '0')
          zipcol   = map (zip [0..])
          ziprow   = zip [0..]
          bringrow = map (\(row,arr) -> zip (repeat row) arr)
          toBoard  = map (\(row,(col,val)) -> ((row,col),val) )

genNeighbour :: Pos -> [Cell]
genNeighbour (row,col) = zip filtered (repeat 1)
    where zipped = zip ( map (take 3 . repeat) [(row-1)..(row+1)]) $ repeat [(col-1)..(col+1)] 
          posses = map (\(a,b) -> zip a b) zipped
          flat   = flatten posses
          filtered = filter (\(r,c) -> elem r [0..9] && elem c [0..9] && (r,c) /= (row,col)) flat

incBoard :: Board -> Board
incBoard [] = []
incBoard ((pos,val):cs) = (pos,val+1) : incBoard cs

combineBoard :: Board -> Board
combineBoard = combin . sort
    where combin :: Board -> Board
          combin [] = []
          combin (x:[]) = [x]
          combin ((pos1,v1):(pos2,v2):rs) 
              | pos1 == pos2 = combin ((pos1,v1+v2):rs)
              | otherwise    = (pos1,v1) : combin ((pos2,v2):rs) 

flashHelper :: Board -> Board -> Board
flashHelper acc [] = acc
flashHelper acc board
    | val <= 9 || val >= 99 = flashHelper (c:acc) cs
    | otherwise = flashHelper (flashacc++acc) cs
    where (c:cs) = board
          ((row,col),val) = c
          flashacc = ((row,col),99) : genNeighbour (row,col)

flashBoard :: Board -> Board
flashBoard board
    | board == newboard = map flashed board
    | otherwise = flashBoard newboard
    where newboard = combineBoard $ flashHelper [] board
          flashed :: Cell -> Cell
          flashed (pos,x)
              | x >= 99   = (pos,0)
              | otherwise = (pos,x)

countFlash :: Board -> Int
countFlash = length . filter (\(_,val) -> val == 0)

stepBoard :: Board -> Board
stepBoard = flashBoard . incBoard

totalFlash :: Int -> Int -> Board -> Int
totalFlash 0 flashc _ = flashc
totalFlash n flashc board = totalFlash (n-1) (flashc+newflash) newboard
    where newboard = stepBoard board
          newflash = countFlash newboard


boardToInt :: Board -> [Int]
boardToInt = map (\(_,x) -> x)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        board = parseInp lin
        step  = 100
    print $ totalFlash step 0 board
