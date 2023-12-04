import System.IO
import Data.List(sort)

type Point = (Int,Int,Int)
type Cube  = (Int,Int,Int)
type Face  = [Point]

parseCube :: String -> Cube
parseCube x = read ("(" ++ x ++ ")")

genFaces :: Cube -> [Face]
genFaces (x,y,z) = sort [fxy, fxy', fyz, fyz', fzx, fzx']
    where (x',y',z') = (x+1, y+1, z+1)
          fxy  = [(x,y,z ), (x+1,y,z ), (x+1,y+1,z ), (x,y+1,z )]
          fxy' = [(x,y,z'), (x+1,y,z'), (x+1,y+1,z'), (x,y+1,z')]
          fyz  = [(x ,y,z), (x ,y+1,z), (x ,y+1,z+1), (x ,y,z+1)]
          fyz' = [(x',y,z), (x',y+1,z), (x',y+1,z+1), (x',y,z+1)]
          fzx  = [(x,y ,z), (x,y ,z+1), (x+1,y ,z+1), (x+1,y ,z)]
          fzx' = [(x,y',z), (x,y',z+1), (x+1,y',z+1), (x+1,y',z)]

removeIntersection :: [Face] -> [Face]
removeIntersection [] = []
removeIntersection (x:[]) = [x]
removeIntersection (x:y:xs)
    | x == y    = removeIntersection xs
    | otherwise = x : removeIntersection (y:xs)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes  = map parseCube lin
        faces  = sort $ concat $ map genFaces cubes
        faces' = removeIntersection faces
    print cubes
    print $ length faces'
