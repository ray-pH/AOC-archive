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
unionableRange r1 r2 = m <= b +1
    where ((a,b), (m,n)) = if r1 < r2 then (r1,r2) else (r2,r1)

constrainRange :: Range -> Range -> Range
constrainRange (ca, cb) (a,b) = (max ca a, min cb b)

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

getSimpRangesatRow :: [BeaconData] -> Int -> [Range]
getSimpRangesatRow dat yrow = simplifyRanges $ sort $ filter validRange $ map (evalNobeaconInRow yrow) dat

-- 6180329 too high
-- 6078701
main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        dat = map parseSensor lin
        -- maxy  = 20
        maxy  = 4000000
        yrows = [0..maxy]
        simpranges   = map (map (constrainRange (0,maxy))) $ map (getSimpRangesatRow dat) yrows
        nonfull      = filter ((/=) [(0,maxy)] . snd) $ zip [0..] simpranges
        (yb, xxx)    = head nonfull
        xb = (+) 1 $ snd $ head xxx
    print dat
    print "------"
    mapM_ print $ zip [0..] simpranges
    print "------"
    print nonfull
    print $ 4000000 * xb + yb
