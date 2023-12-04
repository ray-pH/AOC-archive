import System.IO
import Data.Char

type Board = [[Int]]
type Neigh = [Int]
type NeighBoard = [[Neigh]]

parseInp :: [String] -> Board
parseInp = map (map (\c -> ord c - ord '0' ))

padBoard :: Board -> Board
padBoard board = rowpad : padlr board ++ [rowpad]
    where padlr = map (\row -> [9] ++ row ++ [9])
          len   = 2 + length (board!!0)
          rowpad = take len $ repeat 9

getNeighRow :: [Int] -> [Int] -> [Int] -> [Neigh]
getNeighRow (x:xx:[]) (y:yy:[]) (z:zz:[]) = []
getNeighRow (x:xx:xxx:xs) (y:yy:yyy:ys) (z:zz:zzz:zs) = neigh : getNeighRow (xx:xxx:xs) (yy:yyy:ys) (zz:zzz:zs)
    where neigh = [xx,y,yyy,zz]

getNeighPadded :: Board -> NeighBoard
getNeighPadded (x:y:[]) = []
getNeighPadded (x:y:z:rest) = neighrow : getNeighPadded (y:z:rest)
    where neighrow = getNeighRow x y z

getNeigh :: Board -> NeighBoard
getNeigh = getNeighPadded . padBoard

isLowest :: (Int, Neigh) -> Bool
isLowest (x,neigh) = and $ map (\n -> x < n) neigh

riskLevel :: [(Int,Neigh)] -> Int
riskLevel [] = 0
riskLevel ((x,_):rest) = x + 1 + riskLevel rest

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        board  = parseInp lin
        neighb = getNeigh board
        zipped = map (\(x,y) -> zip x y) $ zip board neighb
        lowest = flatten $ map (filter isLowest) zipped
    print $ riskLevel lowest
