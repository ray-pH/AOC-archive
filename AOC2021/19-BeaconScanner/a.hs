import System.IO
import Data.Maybe
import Debug.Trace

type Vec = (Int,Int,Int)
type Scanner = [Vec]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
    | elem x xs = unique xs
    | otherwise = x : unique xs

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

countSame :: (Eq a) => [a] -> [a] -> Int
countSame [] _ = 0
countSame (x:xs) arr = (if elem x arr then 1 else 0) + countSame xs arr

parseVec :: [Char] -> Vec
parseVec = f . map read . splitBy ','
    where f (x:y:z:_) = (x,y,z)

orientation :: Int -> Vec -> Vec
orientation  0 (x,y,z) = ( x, y, z)
orientation  1 (x,y,z) = ( x, z,-y)
orientation  2 (x,y,z) = ( x,-y,-z)
orientation  3 (x,y,z) = ( x,-z, y)

orientation  4 (x,y,z) = ( y, z, x)
orientation  5 (x,y,z) = ( y, x,-z)
orientation  6 (x,y,z) = ( y,-z,-x)
orientation  7 (x,y,z) = ( y,-x, z)

orientation  8 (x,y,z) = ( z, x, y)
orientation  9 (x,y,z) = ( z, y,-x)
orientation 10 (x,y,z) = ( z,-x,-y)
orientation 11 (x,y,z) = ( z,-y, x)

orientation 12 (x,y,z) = (-x, y,-z)
orientation 13 (x,y,z) = (-x, z, y)
orientation 14 (x,y,z) = (-x,-y, z)
orientation 15 (x,y,z) = (-x,-z,-y)

orientation 16 (x,y,z) = (-y, z,-x)
orientation 17 (x,y,z) = (-y, x, z)
orientation 18 (x,y,z) = (-y,-z, x)
orientation 19 (x,y,z) = (-y,-x,-z)

orientation 20 (x,y,z) = (-z, x,-y)
orientation 21 (x,y,z) = (-z, y, x)
orientation 22 (x,y,z) = (-z,-x, y)
orientation 23 (x,y,z) = (-z,-y,-x)

translate :: Vec -> Vec -> Vec
translate (x,y,z) (x',y',z') = (x+x',y+y',z+z')
inverse :: Vec -> Vec
inverse (x,y,z) = (-x,-y,-z)

getTranslate :: Scanner -> Scanner -> [Vec]
getTranslate sa sb' = unique [(translate va $ inverse vb) | va <- sa, vb <- sb']

getCount :: Scanner -> Scanner -> Vec -> (Int,Vec)
getCount sa sb' tran = ((countSame sa $ map (translate tran) sb'), tran)

getValidTrans :: Scanner -> Scanner -> [(Int,Vec)]
getValidTrans sa sb' = filter (\(x,_) -> x >= 12) $ map (getCount sa sb') trans
    where trans = getTranslate sa sb'

genOrientations :: Scanner -> [Scanner]
genOrientations s = [map (orientation x) s | x <- [0..23]]

getOrientation :: Scanner -> Scanner -> Maybe (Int,Int,Vec) -- (orient,count,trans)
getOrientation sa sb
    | 0 < length filtered = Just (orient,count,trans)
    | otherwise           = Nothing
    where ors = genOrientations sb
          valtrans = map (getValidTrans sa) ors
          filtered = filter ((/=) [] . snd) $ zip [0..23] valtrans
          (orient, arr)  = head filtered
          (count, trans) = head arr

combineScanner :: Scanner -> Scanner -> (Bool,Scanner)
-- combineScanner sa sb = trace (show (or,c,t)) unique (sa ++ sb')
combineScanner sa sb
    | isNothing geor = (False,[])
    | otherwise      = trace (show geor) (True, unique (sa ++ sb') )
    where geor = getOrientation sa sb
          (or,c,t) = f geor
          sb' = map (\v -> translate t $ orientation or v) sb
          f (Just x) = x

doCombine :: [Scanner] -> Int -> Scanner
doCombine [s] _ = s
doCombine (sa:sb:ss) x
    | isCombined = trace (show x) doCombine (scombined:ss) (x+1)
    | otherwise  = doCombine (sa:ss ++ [sb]) x
    where (isCombined, scombined) = combineScanner sa sb

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        splat = splitBy "" lin
        scannersStr = map tail splat
        scanners = map (map parseVec) scannersStr
        scomb = doCombine scanners 1

    print $ length scomb
    -- print sc
    -- print $ translate  (-618,-824,-621) $ inverse (-686,422,-578)
    --                    ^ 0                         ^ 1
    -- print $ translate (68,-1246,-43) $ orientation 12 (553,889,-390)
    --                 ^ trnas                         ^ 1
