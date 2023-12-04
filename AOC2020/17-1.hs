module Main where

import System.IO

data Cube = Cube { x :: Int
                 , y :: Int
                 , z :: Int
                 } deriving Show

data Field = Field { coord :: (Int,Int,Int), value :: Int }
                deriving Show

isInFields :: (Int,Int,Int) -> [Field] -> Bool
isInFields co fields = elem co $ map coord fields

appendField :: (Int,Int,Int) -> [Field] -> [Field]
appendField co fields
  | isInFields co fields = [ if (coord f) == co then incField f else f | f <- fields ]
  | otherwise = (Field co 1) : fields
  where incField :: Field -> Field
        incField f = Field (coord f) (value f + 1)

getCubeFromInitial :: [String] -> [Cube]
getCubeFromInitial st = let h = length st
                            w = length $ head st
                        in [ Cube x y 0 | x<-[0..w-1], y<-[0..h-1], '#' == (st !! y !! x) ]

isThereACube :: (Int,Int,Int) -> [Cube] -> Bool
isThereACube co cubes = or $ map (isCoOfCube co) cubes
        where isCoOfCube :: (Int,Int,Int) -> Cube -> Bool
              isCoOfCube (x',y',z') cube = (x' == cx) && (y' == cy) && (z' == cz)
                            where Cube cx cy cz = cube

getAdjacents :: Cube -> [(Int,Int,Int)]
getAdjacents cb = [(x,y,z) | x <- ar cx, y <- ar cy, z <- ar cz, (x,y,z) /= (cx,cy,cz)]
    where Cube cx cy cz = cb
          ar = (\y -> [y-1 .. y+1])

-- Fill Field
genAllAdjacents :: [Cube] -> [(Int,Int,Int)]
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
                   in [Cube x y z | (x,y,z) <- newcubescoords]

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
        final = doCycle 6 cubes
    mapM_ print inits
    mapM_ print cubes
    print "help"
    -- print $ isThereACube (0,0,0) cubes
    -- print $ length $ genStateOfFields cubes
    print $ length final 
