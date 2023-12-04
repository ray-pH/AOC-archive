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

canConnect :: Face -> Face -> Bool
canConnect f1 f2 = (==) 2 $ length $ filter (flip elem f1) f2

removeIntersection :: [Face] -> [Face]
removeIntersection [] = []
removeIntersection (x:[]) = [x]
removeIntersection (x:y:xs)
    | x == y    = removeIntersection xs
    | otherwise = x : removeIntersection (y:xs)

--               conn   candidate rest
data Builder = B [Face] [Face]    [Face]
getConn      (B c _ _) = c
getCandidate (B _ c _) = c
getRest      (B _ _ r) = r
getList      (B c ca r) = [c, ca, r]

buildFrom :: Builder -> Builder
buildFrom (B conn []     rest) = B conn [] rest
buildFrom (B conn (c:cs) rest)
    | connectable = buildFrom (B (c:conn) cs rest    )
    | otherwise   = buildFrom (B conn     cs (c:rest))
    where connectable = any (canConnect c) conn

restToCandidate :: Builder -> Builder
restToCandidate (B conn [] rest) = B conn rest []

keepBuilding :: Builder -> Builder
keepBuilding b
    | same      = b
    | otherwise = keepBuilding $ restToCandidate b'
    where b' = buildFrom b
          same = (getConn b) == (getConn b')

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes  = map parseCube lin
        faces  = sort $ concat $ map genFaces cubes
        faces' = removeIntersection faces
        b      = B [head faces'] (tail faces') []
        b'     = keepBuilding b
    print cubes
    print $ length faces'
    mapM_ print $ getList b'
