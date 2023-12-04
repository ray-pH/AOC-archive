import System.IO
import Data.List(sort)
import Debug.Trace

type Cube = (Int,Int,Int)
type Pos  = (Int,Int,Int)
type Face  = [Pos]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

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

translate :: Pos -> Pos -> Pos
translate (x,y,z) (p,q,r) = (x+p, y+q, z+r)

parseCube :: String -> Cube
parseCube x = read ("(" ++ x ++ ")")
-- parseCube = (\[x,y,z] -> (read x,read y,read z)) . splitBy ','

findmaxes :: [Cube] -> (Int,Int,Int)
findmaxes cubes = (maxx, maxy, maxz)
    where maxx =  maximum $ map (\(x,y,z) -> x) cubes
          maxy =  maximum $ map (\(x,y,z) -> y) cubes
          maxz =  maximum $ map (\(x,y,z) -> z) cubes

genCubeinReg :: (Int,Int,Int) -> [Cube]
genCubeinReg (mx,my,mz) = [(x,y,z) | x<-[0..mx], y<-[0..my], z<-[0..mz]]

--                board  explorers explored frees traps limits
data Explorer = E [Cube] [Pos]     [Pos]    [Pos] [Pos] (Int,Int,Int)
getExp (E _ e _ _ _ _) = e

genPossibleMove :: [Cube] -> [Pos] -> Pos -> [Pos]
genPossibleMove cubes explored pos = fnotexp $ fnocube neighbors
    where neighbors = map (translate pos) [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]
          fnocube   = filter (not . flip elem cubes) 
          fnotexp   = filter (not . flip elem explored)

isFree :: Pos -> (Int,Int,Int) -> Bool
isFree (x,y,z) (mx,my,mz) = toosmall || toobig
    where toosmall = x < 0  || y < 0  || z < 0
          toobig   = x > mx || y > my || z > mz

--                       isFree, group
checkFree :: Explorer -> (Bool,[Pos])
checkFree (E cubes [] explrd freePs traPs lims) = (False, explrd)
checkFree (E cubes exps explrd freePs traPs lims)
    | anyfree    = (True, explrd ++ toexplore)
    | anytraps   = (False, explrd ++ toexplore)
    -- | (length toexplore) > 10000 = (False, explrd ++ toexplore)
    | otherwise  = trace (show $ length toexplore) checkFree (E cubes toexplore (explrd ++ exps) freePs traPs lims)
    where toexplore = unique $ concat $ map (genPossibleMove cubes explrd) exps
          anyfree   = any (flip isFree lims) toexplore || any (flip elem freePs) toexplore
          anytraps  = any (flip elem traPs) toexplore
          -- anyfree   = any (flip isFree lims) toexplore 

--                cubes  lim candidate free  trapped
data Searcher = S [Cube] Pos [Pos]     [Pos] [Pos]   
getFree (S _ _ _ f t) = f
getTrap (S _ _ _ f t) = t

doSearch :: Searcher -> Searcher
doSearch (S cubes lims [] freePs trapPs) = (S cubes lims [] freePs trapPs)
doSearch (S cubes lims (c:cs) freePs trapPs)
    | isfree     = doSearch (S cubes lims cs' (freePs++neighs) trapPs)
    | not isfree = doSearch (S cubes lims cs' freePs           (trapPs++neighs))
    where (isfree,neighs) = checkFree (E cubes [c] [] freePs trapPs lims)
          cs0 = filter (not . flip elem neighs) cs --remove neighs
          cs' = trace (show (length cs0) ++ show c) cs0

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes = map parseCube lin
        allpoint   = genCubeinReg $ findmaxes cubes
        allvoid    = filter (not . flip elem cubes) allpoint
        -- candidates = [(3,9,9)]
        candidates = allvoid
        searcher   = S cubes (findmaxes cubes) candidates [] []
        searcher'  = doSearch searcher
        trapped    = getTrap searcher'
        cubes'     = cubes ++ trapped
        faces      = sort $ concat $ map genFaces cubes'
        faces'     = removeIntersection faces
        (tisf, tne)= checkFree (E cubes candidates [] [] [] (findmaxes cubes))
--                board  explorers explored frees traps limits
    -- print cubes
    -- print $ getFree searcher'
    --
    -- print $ findmaxes cubes
    -- print $ (tisf, length tne)
    --
    print $ getTrap searcher'
    print $ length faces'
