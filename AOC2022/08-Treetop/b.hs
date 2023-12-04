import System.IO
import Data.List

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

type Coord = (Int,Int)
type Map   = [String]

countVisible :: String -> Int
countVisible (initial:rest) = (length left) + (notEmptyRight)
    where (left,right) = span (\x -> x < initial) rest
          notEmptyRight = if right == "" then 0 else 1

genLeft :: Coord -> Map -> String
genLeft (row,col) mp = reverse $ take (col+1) $ mp !! row
genRight :: Coord -> Map -> String
genRight (row,col) mp = drop col $ mp !! row
genUp :: Coord -> Map -> String
genUp (row,col) mp = reverse $ take (row+1) $ (transpose mp) !! col
genDown :: Coord -> Map -> String
genDown (row,col) mp = drop row $ (transpose mp) !! col

genViewlines :: Coord -> Map -> [String]
genViewlines coord mp = map (\f -> f coord mp) [genUp, genLeft, genRight, genDown]

evaluateCoord :: Coord -> Map -> Int
evaluateCoord coord mp = foldr (*) 1 $ map countVisible viewlines
    where viewlines = genViewlines coord mp

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        height = length lin
        width  = length $ lin !! 0
        coords = [(y,x) | x <- [0..(width-1)], y<- [0..(height-1)]]
        evaluated = map (\c -> evaluateCoord c lin) coords
    mapM_ print lin
    print evaluated
    print $ foldr max 0 evaluated
