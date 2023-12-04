import System.IO
import Data.List
import Data.Char

type Board     = [String]
type BoolBoard = [[Bool]]
type Pos       = (Int,Int)
--                                  h,w       explrs  target Counter
data BData     = BD Board BoolBoard (Int,Int) [Pos]   Pos    Int     deriving Show

getCounter (BD _ _ _ _ _ c) = c
getExplrs  (BD _ _ _ e _ _) = e
bustCounter :: BData -> BData
bustCounter (BD board boolboard (h,w) xplrs targetPos c) = (BD board boolboard (h,w) xplrs targetPos (-1))

indexesOf :: Eq a => a -> [a] -> [Int]
indexesOf v = map fst . filter ((== v) . snd) . zip [0..]

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt id val arr = l ++ [val] ++ r
    where (l, (x:r)) = splitAt id arr

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

isNothing :: Maybe Int -> Bool
isNothing Nothing  = False
isNothing (Just _) = True

getJust :: Maybe Int -> Int
getJust (Just x) = x

findStart :: Board -> Pos
findStart = (\(r,c) -> (r, getJust c)) . head . filter (\(row,id) -> isNothing id) . zip [0..] . map (elemIndex 'S')
findEnd :: Board -> Pos
findEnd = (\(r,c) -> (r, getJust c)) . head . filter (\(row,id) -> isNothing id) . zip [0..] . map (elemIndex 'E')

-- (0,[1,2]) -> [(0,1), (0,2)]
expandTuple :: (a,[b]) -> [(a,b)]
expandTuple (i,xs) = [(i,x) | x <- xs]
findAllAs :: Board -> [Pos]
findAllAs = flatten . map expandTuple . filter (\(_,arr) -> length arr > 0 ). zip [0..] . map (indexesOf 'a')

genBoardWithStart :: Board -> Pos -> BData
genBoardWithStart board startPos = BD board boolB (height,width) [startPos] endPos 0
    where height   = length board
          width    = length $ head board
          boolrow  = map (const False) [1..width]
          boolB    = map (const boolrow) [1..height]
          endPos   = findEnd board

exploreBoardSingle :: Pos -> BoolBoard -> BoolBoard
exploreBoardSingle (r,c) boolboard = replaceAt r row' boolboard
    where row' = replaceAt c True (boolboard !! r)

exploreBoard :: [Pos] -> BoolBoard -> BoolBoard
exploreBoard []     boolboard = boolboard
exploreBoard (p:ps) boolboard = exploreBoard ps $ exploreBoardSingle p boolboard

genNeighbours :: Pos -> [Pos]
genNeighbours (r,c) = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

getPossibleDirection :: BData -> Pos -> [Pos]
getPossibleDirection (BD board boolboard (h,w) _ _ _) pos = validPos
    where neighbours  = genNeighbours pos
          validPos    = filter (canClimb board pos) $ filter (hasNotVisited boolboard) $ filter (isInside (h,w)) neighbours
          isInside :: (Int,Int) -> Pos -> Bool
          isInside (h,w) (r,c)
              | r < 0 || c < 0 = False
              | r >= h         = False
              | c >= w         = False
              | otherwise      = True
          hasNotVisited :: BoolBoard -> Pos -> Bool
          hasNotVisited bb (r,c) = not $ (bb !! r) !! c
          canClimb :: Board -> Pos -> Pos -> Bool
          canClimb board (fromr, fromc) (tor, toc) =  atStart || atEnd || climbable
              where fromCh = (board !! fromr) !! fromc
                    toCh   = (board !! tor)   !! toc
                    climbable = ((ord toCh) - (ord fromCh) <= 1) && (toCh /= 'E')
                    atStart   = (fromCh == 'S' && toCh == 'a')
                    atEnd     = (fromCh == 'y' && toCh == 'E') || (fromCh == 'z' && toCh == 'E')

stepExplore :: BData -> BData
stepExplore (BD board boolboard  (h,w) xplrs  targetPos c)
        =   (BD board boolboard' (h,w) xplrs' targetPos (c+1))
        where bdata      = (BD board boolboard (h,w) xplrs targetPos c)
              xplrs'     = unique $ flatten $ map (getPossibleDirection bdata) xplrs
              boolboard' = exploreBoard xplrs' boolboard

isDone :: BData -> Bool
isDone (BD _ boolboard _ _ (targetr, targetc) _) = (boolboard !! targetr) !! targetc

doExplore :: BData -> BData
doExplore bdata
    | isDone bdata = bdata
    | length (getExplrs bdata) == 0 = bustCounter bdata
    | otherwise    = doExplore $ stepExplore bdata

doExploreGetCount :: BData -> Int
doExploreGetCount = getCounter . doExplore

replaceS :: [String] -> [String]
replaceS strarr = replaceAt r row' strarr
    where (r,c) = findStart strarr
          row   = strarr !! r
          row'  = replaceAt c 'a' row

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin  = lines cont
        lin' = replaceS lin
        starts = findAllAs lin'
        boards = map (genBoardWithStart lin) starts
        counts = map doExploreGetCount boards
        -- (hist,board') = doExploreHist ([], board)
    mapM_ print lin
    print "----"
    mapM_ print lin'
    print starts
    print counts
    print "----"
    print $ sort $ filter (\x -> x /= (-1)) counts
    -- mapM_ print hist
