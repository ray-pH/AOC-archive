import System.IO

data Axis = X | Y
    deriving (Show)
type Pos = (Int,Int)
type Fold = (Axis,Int)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
    | elem x xs = unique xs
    | otherwise = x : unique xs

parsePos :: String -> Pos
parsePos = toPos . map read . words . map commaToSpace
    where commaToSpace :: Char -> Char
          commaToSpace ',' = ' '
          commaToSpace x = x
          toPos :: [Int] -> (Int,Int)
          toPos (x:y:[]) = (x,y)

parseFold :: String -> Fold
parseFold = toFold . last . words
    where toFold :: String -> Fold
          toFold ('x':'=':rs) = (X, read rs)
          toFold ('y':'=':rs) = (Y, read rs)

-- d = y - yval
-- y' = yval - d = 2*yval - y

fold :: Fold -> [Pos] -> [Pos]
fold (X,xval) = unique . map (fx xval)
    where fx :: Int -> Pos -> Pos
          fx xval (x,y)
              | x > xval  = (2*xval - x, y)
              | otherwise = (x,y)
fold (Y,yval) = unique . map (fy yval)
    where fy :: Int -> Pos -> Pos
          fy yval (x,y)
              | y > yval  = (x, 2*yval - y)
              | otherwise = (x,y)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        (coordsStr, _:foldsStr) = span ((/=) "") lin
        dots  = map parsePos coordsStr :: [Pos]
        folds = map parseFold foldsStr :: [Fold]
        fir   = fold (folds !! 0) dots
    print $ length fir
