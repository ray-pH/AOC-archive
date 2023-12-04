import System.IO

type Cube = (Int,Int,Int)

subabs :: Cube -> Cube -> Cube
subabs (x,y,z) (p,q,r) = (abs (x-p), abs (y-q), abs (z-r))
isSharingFace :: Cube -> Cube -> Bool
isSharingFace c1 c2 = sa == (0,0,1) || sa == (0,1,0) || sa == (1,0,0)
    where sa = subabs c1 c2

parseCube :: String -> Cube
parseCube x = read ("(" ++ x ++ ")")

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

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        cubes = map parseCube lin
    print cubes
    print $ calcFaceArea cubes
