import System.IO

type Cube = (Int,Int,Int)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

subabs :: Cube -> Cube -> Cube
subabs (x,y,z) (p,q,r) = (abs (x-p), abs (y-q), abs (z-r))
isSharingFace :: Cube -> Cube -> Bool
isSharingFace c1 c2 = sa == (0,0,1) || sa == (0,1,0) || sa == (1,0,0)
    where sa = subabs c1 c2

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
findAirTrapped cubes = filter (not . flip elem cubes) $ filter ((==) 6 . flip getFaceSharedCount cubes) allpoint
    where allpoint = genCubeinReg $ findmaxes cubes

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes = map parseCube lin
        airtrapped = findAirTrapped cubes
        cubes' = cubes ++ airtrapped
    print cubes
    print "airtrapped :"
    print airtrapped
    print $ calcFaceArea cubes'
