import System.IO
import Debug.Trace

data State = On | Off deriving (Show, Eq)
type Range = (Int,Int)
type Cube = [Range]
type Instructon = (State,Cube)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

parseLine :: String -> Instructon
parseLine str = (state, components)
    where (stateStr:ranges:[]) = words str
          state = if stateStr == "on" then On else Off
          components = map parseRange $ map (splitBy '.') $ map (tail . tail) $ splitBy ',' ranges
          parseRange :: [String] -> Range
          parseRange (a:"":b:[]) = (read a, read b)
          -- toCube :: [Range] -> Cube
          -- toCube (x:y:z:[]) = (x,y,z)

validRange :: Range -> Bool
validRange (a,b) = a <= b

validCube :: Cube -> Bool
validCube = and . map validRange

intersectRange :: Range -> Range -> Range
intersectRange (a1,b1) (a2,b2) = ((max a1 a2), (min b1 b2))

intersectCube :: Cube -> Cube -> Cube
intersectCube ca cb = map (\(a,b) -> intersectRange a b) $ zip ca cb

getLength' :: Range -> Int
getLength' (a,b) = b - a + 1
getVolume :: Cube -> Int
getVolume = product . map getLength'

--
-- off ->   -(x,+) +(x,-)
-- on -> +x -(x,+) +(x,-)
--
invsignIntersect :: Cube -> (Int,Cube) -> (Int,Cube)
invsignIntersect cube (s,c) = (-1*s, intersectCube cube c)

convertInstruction :: [Instructon] -> [(Int,Cube)] -> [(Int,Cube)]
convertInstruction [] stack = stack
convertInstruction (x:xs) stack
    | On  == fst x = convertInstruction xs (stack' ++ [(1,snd x)])
    | Off == fst x = convertInstruction xs stack'
    where invint = filter (validCube . snd) $ map (invsignIntersect (snd x)) stack :: [(Int,Cube)]
          stack' = stack ++ invint

getValue :: [(Int,Cube)] -> Int
getValue [] = 0
getValue ((s,c):xs) = s * (getVolume c) + getValue xs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpexb.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        insts = map parseLine lin
        converted = convertInstruction insts []
        result = getValue converted
    -- mapM_ print insts
    mapM_ print converted
    print result

-- on 1
-- on 2
-- off 3
-- on 4
--
-- +1
-- +1 +2 -a(1,2)
-- +1 +2 -a -b(1,3) -c(2,3) +d(a,3)
-- +1 +2 -a -b -c +d +4 -(1,4) -(1,2) +(a,4) +(b,4) +(c,4) -(d,4)
--
-- off ->   -(x,+) +(x,-)
-- on -> +x -(x,+) +(x,-)
