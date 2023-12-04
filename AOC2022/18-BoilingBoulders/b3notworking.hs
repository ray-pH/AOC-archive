import System.IO

type Cube = (Int,Int,Int)
type Pos  = (Int,Int,Int)

subabs :: Cube -> Cube -> Cube
subabs (x,y,z) (p,q,r) = (abs (x-p), abs (y-q), abs (z-r))
isSharingFace :: Cube -> Cube -> Bool
isSharingFace c1 c2 = sa == (0,0,1) || sa == (0,1,0) || sa == (1,0,0)
    where sa = subabs c1 c2

translate :: Pos -> Pos -> Pos
translate (x,y,z) (p,q,r) = (x+p, y+q, z+r)

parseCube :: String -> Cube
parseCube x = read ("(" ++ x ++ ")")
-- parseCube = (\[x,y,z] -> (read x,read y,read z)) . splitBy ','

countFaceShared :: [Cube] -> Int
countFaceShared [] = 0
countFaceShared (c:cs) = (+) rest $ sum $ map (fromEnum . isSharingFace c) cs
    where rest = countFaceShared cs

calcFaceArea :: [Cube] -> Int
calcFaceArea cubes = rawarea - sharedarea
    where rawarea    = 6 * length cubes
          sharedarea = 2 * countFaceShared cubes

getFaceSharedCount :: Cube -> [Cube] -> Int
getFaceSharedCount c cs = sum $ map (fromEnum . isSharingFace c) [x | x<-cs, x /=c ]

findmaxes :: [Cube] -> (Int,Int,Int)
findmaxes cubes = (maxx, maxy, maxz)
    where maxx =  maximum $ map (\(x,y,z) -> x) cubes
          maxy =  maximum $ map (\(x,y,z) -> y) cubes
          maxz =  maximum $ map (\(x,y,z) -> z) cubes

genCubeinReg :: (Int,Int,Int) -> [Cube]
genCubeinReg (mx,my,mz) = [(x,y,z) | x<-[0..mx], y<-[0..my], z<-[0..mz]]

findAirTrapped :: [Cube] -> [Cube]
findAirTrapped cubes = filter ((==) 6 . flip getFaceSharedCount cubes) $ filter (not . flip elem cubes) allpoint
    where allpoint = genCubeinReg $ findmaxes cubes

--                board  explorers explored limits
data Explorer = E [Cube] [Pos]     [Pos]    (Int,Int,Int)
getExp (E _ e _ _) = e

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
checkFree (E cubes [] explrd lims) = (False, explrd)
checkFree (E cubes exps explrd lims)
    | anyfree    = (True, explrd ++ toexplore)
    | otherwise  = checkFree (E cubes toexplore (explrd ++ exps) lims)
    where toexplore = concat $ map (genPossibleMove cubes explrd) exps
          anyfree   = any (flip isFree lims) toexplore

--                cubes  lim candidate free  trapped
data Searcher = S [Cube] Pos [Pos]     [Pos] [Pos]   
getFree (S _ _ _ f t) = f
getTrap (S _ _ _ f t) = t

doSearch :: Searcher -> Searcher
doSearch (S cubes lims [] freePs trapPs) = (S cubes lims [] freePs trapPs)
doSearch (S cubes lims (c:cs) freePs trapPs)
    | isfree     = doSearch (S cubes lims cs' (freePs++neighs) trapPs)
    | not isfree = doSearch (S cubes lims cs' freePs           (trapPs++neighs))
    where (isfree,neighs) = checkFree (E cubes [c] [] lims)
          cs' = filter (not . flip elem neighs) cs --remove neighs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes = map parseCube lin
        allpoint   = genCubeinReg $ findmaxes cubes
        allvoid    = filter (not . flip elem cubes) allpoint
        -- candidates = [(2,2,5), (1,1,1), (3,3,3), (1,1,2)]
        candidates = allvoid
        searcher   = S cubes (findmaxes cubes) candidates [] []
        searcher'  = doSearch searcher
        trapped    = getTrap searcher'
        cubes'     = cubes ++ trapped
    -- print cubes
    -- print $ getFree searcher'
    print trapped
    print $ calcFaceArea cubes'
