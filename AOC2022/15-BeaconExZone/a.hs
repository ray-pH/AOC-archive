import System.IO
import Data.List(sort)

type BeaconData = ((Int,Int), (Int,Int))
type Range = (Int,Int)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

parseSensor :: String -> BeaconData
parseSensor str = ((x0',y0'), (x1',y1'))
    where [x0,y0,x1,y1] = filter (elem '=') $ words str
          x0' = read $ last $ splitBy '=' $ init x0 :: Int
          y0' = read $ last $ splitBy '=' $ init y0 :: Int
          x1' = read $ last $ splitBy '=' $ init x1 :: Int
          y1' = read $ last $ splitBy '=' y1 :: Int

manhattanDist :: BeaconData -> Int
manhattanDist ((xs,ys), (xb,yb)) = (abs (xs - xb)) + (abs (ys - yb))
-- d = |xs - xb| + |ys - yb|
-- d_ <= d
-- |x - xs| + |y - ys| <= d
-- |x - xs| <= d - |y - ys|
-- |y - ys| - d + xs <= x <= d - |y - ys| + xs
evalNobeaconInRow :: Int -> BeaconData -> Range
evalNobeaconInRow y ((xs,ys), (xb,yb)) = (l,r)
    where mdist = manhattanDist ((xs,ys), (xb,yb))
          v     = mdist - abs (y - ys)
          [l,r] = [-v + xs,v + xs]

unionableRange :: Range -> Range -> Bool
unionableRange r1 r2 = m <= b
    where ((a,b), (m,n)) = if r1 < r2 then (r1,r2) else (r2,r1)

unionRange :: Range -> Range -> Range
unionRange r1 r2
  | r1 < r2 && unionableRange r1 r2 = (a,max b n)
  where ((a,b),(m,n)) = (r1,r2)

simplifyRanges :: [Range] -> [Range]
simplifyRanges [] = []
simplifyRanges (x:[]) = [x]
simplifyRanges (x:y:xs)
    | unionableRange x y = simplifyRanges ((unionRange x y) : xs)
    | otherwise          = x : simplifyRanges (y:xs)

validRange :: Range -> Bool
validRange (a,b) = b >= a

isInRange :: Range -> Int -> Bool
isInRange (a,b) x = a <= x && x <= b

isInRanges :: [Range] -> Int -> Bool
isInRanges ranges x = or $ map (\r -> isInRange r x) ranges

rangeLength :: Range -> Int
rangeLength (a,b) = b-a+1

-- 6180329 too high
-- 6078701
main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        dat = map parseSensor lin
        -- yrow = 100
        yrow = 2000000
        nobeaconsranges = sort $ filter validRange $ map (evalNobeaconInRow yrow) dat
        simpranges   = simplifyRanges nobeaconsranges
        rangelengths = map rangeLength simpranges
        xbeaconaty = map fst $ unique $ filter ((==) yrow . snd) $ map snd dat
        toremove  = filter (isInRanges simpranges) xbeaconaty
    print dat
    print "------"
    print nobeaconsranges
    print "------"
    print simpranges
    print rangelengths
    print toremove
    print $ (sum rangelengths) - (length toremove)
    -- print nobeacons
    -- print toremove
    -- print $ (length nobeacons) - (length toremove)
