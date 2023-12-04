import System.IO
import Debug.Trace

type Coord   = (Int,Int)
type Segment = (Coord,Coord)

parseLine :: String -> Segment
parseLine line = ((x1,y1), (x2,y2))
    where [p1,_,p2] = words line
          [x1,y1] = f p1
          [x2,y2] = f p2
          commaToSpace :: Char -> Char
          commaToSpace ',' = ' '
          commaToSpace x = x
          f = map (read :: String -> Int) . words . map commaToSpace

-- unwrapSegment :: Segment -> [Coord]
-- unwrapSegment ((x1, y1),(x2, y2))
--     | y1 == y2  = zip [(min x1 x2)..(max x1 x2)] [y1,y1..] -- Horizontal
--     | x1 == x2  = zip [x1,x1..] [(min y1 y2)..(max y1 y2)] -- Vertical
--     | otherwise = []

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

pairSingle :: [a] -> [(a,a)]
pairSingle [] = []
pairSingle (x:xs) = zip (repeat x) xs ++ pairSingle xs

pairDouble :: [a] -> [a] -> [(a,a)]
pairDouble [] _ = []
pairDouble (x:xs) arr = zip (repeat x) arr ++ pairDouble xs arr

countOverlap :: Int -> Int -> Int -> Int -> [Int]
countOverlap x y a b
    | (x <= a) && (a <= y) = [a..(min y b)]
    | (a <= x) && (x <= b) = [x..(min y b)]
    | otherwise = []

countHorizontal :: [(Segment,Segment)] -> [Coord]
countHorizontal [] = []
countHorizontal (sg:sgs)
    | y1 == ya  = zip ( countOverlap (min x1 x2) (max x1 x2) (min xa xb) (max xa xb) ) (repeat y1) ++ next
    | otherwise = next
    where (s1,s2) = sg
          ((x1,y1),(x2,y2)) = s1
          ((xa,ya),(xb,yb)) = s2
          next = countHorizontal sgs

countVertical :: [(Segment,Segment)] -> [Coord]
countVertical [] = []
countVertical (sg:sgs)
    | x1 == xa  = zip (repeat x1) ( countOverlap (min y1 y2) (max y1 y2) (min ya yb) (max ya yb) ) ++ next
    | otherwise = next
    where (s1,s2) = sg
          ((x1,y1),(x2,y2)) = s1
          ((xa,ya),(xb,yb)) = s2
          next = countVertical sgs

countCombination :: [(Segment,Segment)] -> [Coord]
countCombination [] = []
countCombination (sg:sgs)
    | (min ya yb <= y1) && (y1 <= max ya yb) = (xa,y1) : next
    | otherwise = next
    where (s1,s2) = sg
          ((x1,y1),(x2,y2)) = s1 --horizontal
          ((xa,ya),(xb,yb)) = s2 --vertical
          next = countCombination sgs

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin  = lines cont
        segs = map parseLine lin
        hor = filter (\((x1,y1),(x2,y2)) -> (y1 == y2)) segs
        ver = filter (\((x1,y1),(x2,y2)) -> (x1 == x2)) segs
        chor = countHorizontal $ pairSingle hor
        cver = countVertical $ pairSingle ver
        ccom = countCombination $ pairDouble hor ver
        double = unique $ chor ++ cver ++ ccom
    -- print double
    print $ length double
