import System.IO

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

sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

unwrapSegment :: Segment -> [Coord]
unwrapSegment ((x1, y1),(x2, y2)) = zip [x1,(x1 + sign (x2-x1) )..x2][y1,(y1 + sign (y2-y1) )..y2]

multiple :: Eq a => [a] -> [a]
multiple [] = []
multiple (x:xs)
    | elem x xs = x : multiple xs
    | otherwise = multiple xs

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin  = lines cont
        segs = map parseLine lin
        horver = filter (\((x1,y1),(x2,y2)) -> (y1 == y2)||(x1 == x2)) segs
        allpoint   = foldr (++) [] $ map unwrapSegment horver
        doubles    = unique $ multiple allpoint
    print doubles
    print $ length doubles
