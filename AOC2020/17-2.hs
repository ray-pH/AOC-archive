module Main where

import System.IO

data Cube = Cube { x :: Int
                 , y :: Int
                 , z :: Int
                 , w :: Int
                 } deriving Show

data Field = Field { coord :: (Int,Int,Int,Int), value :: Int }
                deriving Show

isInFields :: (Int,Int,Int,Int) -> [Field] -> Bool
isInFields co fields = elem co $ map coord fields

appendField :: (Int,Int,Int,Int) -> [Field] -> [Field]
appendField co fields
  | isInFields co fields = [ if (coord f) == co then incField f else f | f <- fields ]
  | otherwise = (Field co 1) : fields
  where incField :: Field -> Field
        incField f = Field (coord f) (value f + 1)

getCubeFromInitial :: [String] -> [Cube]
getCubeFromInitial st = let h = length st
                            w = length $ head st
                        in [ Cube x y 0 0 | x<-[0..w-1], y<-[0..h-1], '#' == (st !! y !! x) ]

isThereACube :: (Int,Int,Int,Int) -> [Cube] -> Bool
isThereACube co cubes = or $ map (isCoOfCube co) cubes
        where isCoOfCube :: (Int,Int,Int,Int) -> Cube -> Bool
              isCoOfCube (x',y',z',w') cube = (x'==cx) && (y'==cy) && (z'==cz) && (w'==cw)
                            where Cube cx cy cz cw = cube

getAdjacents :: Cube -> [(Int,Int,Int,Int)]
getAdjacents cb = [(x,y,z,w)| x<-ar cx, y<-ar cy, z<-ar cz, w<-ar cw, (x,y,z,w)/=(cx,cy,cz,cw)]
    where Cube cx cy cz cw = cb
          ar = (\y -> [y-1 .. y+1])

-- Fill Field
genAllAdjacents :: [Cube] -> [(Int,Int,Int,Int)]
genAllAdjacents [] = []
genAllAdjacents (c:cs) = (getAdjacents c) ++ (genAllAdjacents cs)

genField :: [Cube] -> [Field]
genField cubes = let adjs = genAllAdjacents cubes
                  in foldr (appendField) [] adjs

genStateOfFields :: [Cube] -> [(Bool,Field)]
genStateOfFields cubes = let fields = genField cubes
                          in [ (isThereACube (coord f) cubes ,f) | f <- fields ]

rule :: Bool -> Int -> Bool
rule True val = val == 2 || val == 3
rule False val = val == 3
-- rule cubeexist val

newCubes :: [(Bool,Field)] -> [Cube]
newCubes states = let newcubescoords = [ coord f | (b,f) <- states, rule b $ value f ]
                   in [Cube x y z w | (x,y,z,w) <- newcubescoords]

doCycle :: Int -> [Cube] -> [Cube]
doCycle 0 cubes = cubes
doCycle n cubes = doCycle (n-1) $ newCubes $ genStateOfFields cubes

main :: IO()
main = do
    file <- openFile "17-input.txt" ReadMode
    -- file <- openFile "17-ex.txt" ReadMode
    cont <- hGetContents file
    let inits = lines cont
        cubes = getCubeFromInitial inits
        f = (newCubes . genStateOfFields)
        -- final = doCycle 6 cubes
    mapM_ print inits
    mapM_ print cubes
    let c1 = f cubes 
    print "c1"
    print $ length c1
    let c2 = f c1 
    print "c2"
    print $ length c2
    let c3 = f c2 
    print "c3"
    print $ length c3
    let c4 = f c3 
    print "c4"
    print $ length c4
    let c5 = f c4 
    print "c5"
    print $ length c5
    let c6 = f c5 
    print "c6"
    print $ length c6
    -- print $ isThereACube (0,0,0) cubes
    -- print $ length $ genStateOfFields cubes
    -- print $ length final 
