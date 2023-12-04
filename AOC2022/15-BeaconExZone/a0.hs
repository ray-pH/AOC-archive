import System.IO
import Data.List(sort)

type BeaconData = ((Int,Int), (Int,Int))

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
evalNobeaconInRow :: Int -> BeaconData -> [Int]
evalNobeaconInRow y ((xs,ys), (xb,yb)) = [(-v + xs)..(v + xs)]
    where mdist = manhattanDist ((xs,ys), (xb,yb))
          v     = mdist - abs (y - ys)

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        dat = map parseSensor lin
        yrow = 10
        -- yrow = 2000000
        xbeaconaty = map fst $ unique $ filter ((==) yrow . snd) $ map snd dat
        nobeacons = sort $ unique $ flatten $ map (evalNobeaconInRow yrow) dat
        toremove  = filter (\x -> elem x nobeacons) xbeaconaty
    print dat
    print nobeacons
    print toremove
    print $ (length nobeacons) - (length toremove)
    print "-----"
    mapM_ print $ map (evalNobeaconInRow yrow) dat
