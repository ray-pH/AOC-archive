import System.IO
import Data.List(elemIndex, sort, sortBy)
import Data.Maybe(fromJust)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

iterN :: Int -> (a->a) -> a -> a
iterN 0 _ val = val
iterN n f val = iterN (n-1) f (f val)

--             Id  Flow Conn
data Valve = V Int Int  [Int] deriving Show
--             Vaves   pos opened released
data Board = B [Valve] Int [Int]  Int
data Move  = Goto Int | Open Int            deriving Show

instance Show Board where
    show (B _ p o r) = "B " ++ show (p,o,r)
instance Eq Board where
    (==) (B _ p1 o1 r1) (B _ p2 o2 r2) = (==) (p1:o1) (p2:o2) && r1 == r2
instance Ord Board where
    compare (B _ p1 o1 r1) (B _ p2 o2 r2) = if comp == EQ then compare r1 r2 else comp
        where comp = compare (p1:o1) (p2:o2)

simBoard :: Board -> Board -> Bool
simBoard (B _ p1 o1 _) (B _ p2 o2 _) = p1 == p2 && o1 == o2

getValveConn (V _ _ conn) = conn
getValveFlow (V _ flow _) = flow
isDeadValve = (==) 0 . getValveFlow :: (Valve -> Bool)

getOpened    (B _ _ o _) = o
getPos       (B _ p _ _) = p
getRe        (B _ _ _ r) = r

getValveName :: String -> String
getValveName = flip (!!) 1 . words

parseValve :: [String] -> String -> Valve
parseValve names str = V id flow conn
    where w    = words str
          name = w !! 1
          id   = fromJust $ elemIndex name names
          flow = read $ flip (!!) 1 $ splitBy '=' $ init $ w !! 4 :: Int
          conn = map (fromJust . flip elemIndex names) $ map cleanComma $ drop 9 w
          cleanComma s = if last s == ',' then init s else s

getValidMoves :: Board -> [Move]
getValidMoves (B valves pos opened re)
  | length valves == length opened = []
  | otherwise = if isDeadValve currvalve || iscurrOpened then gomove else (Open pos) : gomove
    where currvalve    = valves !! pos
          gomove       = map (\x -> Goto x) $ getValveConn currvalve
          iscurrOpened = elem pos opened

doMove :: Board -> Move -> Board
doMove (B valves pos opened re) (Goto pos') = (B valves pos' opened re)
doMove (B valves pos opened re) (Open p)
  | pos == p = (B valves pos (sort (p:opened)) re)

relasePressure :: Board -> Board
relasePressure (B valves pos opened re) = (B valves pos opened (re+released))
    where released = sum $ map getValveFlow $ map ((!!) valves) opened :: Int

exploreBoard :: Board -> [Board]
exploreBoard (B valves pos opened re) = map (doMove board) validmoves
    where board = (B valves pos opened re)
          validmoves = getValidMoves board

-- if same state exists, the lower pressure release will bi discarded
-- assume already sorted, b1 < b2
simplifyBoards1 :: [Board] -> [Board]
simplifyBoards1 [] = []
simplifyBoards1 (b:[]) = [b]
simplifyBoards1 (b1:b2:bs)
  | simBoard   b1 b2 = simplifyBoards1 (b2:bs)
  | otherwise        = b1 : simplifyBoards1 (b2:bs)

simplifyBoards2 :: [Board] -> [Board]
simplifyBoards2 = flatten . map gulpBoards . groupBoardByPos

simplifyBoards :: [Board] -> [Board]
simplifyBoards = simplifyBoards2 . simplifyBoards1

-- sublist [1,5] <= [1,2,5]
isSubList :: Ord a => [a] -> [a] -> Bool
isSubList x y = (length x) < (length y) && all (flip elem y) x
isGulpable :: Board -> Board -> Bool
isGulpable (B _ p1 o1 r1) (B _ p2 o2 r2) = isSubList o1 o2 && r1 < r2

-- gulp assuming ..v.. and already sorted by length
fgulpBoards :: [Board] -> [Board]
fgulpBoards [] = []
fgulpBoards (x:xs) 
  | any (isGulpable x) xs = fgulpBoards xs
  | otherwise             = x : fgulpBoards xs
-- gulp assuming already grouped (all have same pos)
gulpBoards :: [Board] -> [Board]
gulpBoards = fgulpBoards . sortBy (\b1 b2 -> compare (length $ getOpened b1) (length $ getOpened b2))

groupBoardByPos :: [Board] -> [[Board]]
groupBoardByPos [] = []
groupBoardByPos boards = curr : groupBoardByPos rest
    where pos         = getPos $ head boards
          (curr,rest) = span ((>=) pos . getPos) boards

stepBoards :: [Board] -> [Board]
stepBoards boards = sort explored
    where afterrelease = map relasePressure boards
          explored = flatten $ map exploreBoard afterrelease

stepandSimBoards :: [Board] -> [Board]
stepandSimBoards = simplifyBoards . stepBoards

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin    = lines cont
        names  = map getValveName lin
        idAA   = fromJust $ elemIndex "AA" names :: Int
        valves = map (parseValve names) lin
        board  = B valves idAA [] 0 :: Board
        boards = [board]
        boards'  = iterN 20 stepandSimBoards boards
        -- boards'  = iterN 0 stepandSimBoards boards
        boards21 = stepandSimBoards boards'
        boards22 = stepandSimBoards boards21
        boards23 = stepandSimBoards boards22
        boards24 = stepandSimBoards boards23
        boards25 = stepandSimBoards boards24
        boards26 = stepandSimBoards boards25
        boards27 = stepandSimBoards boards26
        boards28 = stepandSimBoards boards27
        boards29 = stepandSimBoards boards28
        boards30 = stepandSimBoards boards29
        -- released = map getRe boards'
        getmaxreleased = foldr max 0 . map getRe
    -- mapM_ print $ zip [0..] $ map (map (\x -> (getOpened x, getRe x))) $ groupBoardByPos boards'
    -- mapM_ print $ groupBoardByPos boards'
    -- print $ length boards'
    print (20, length boards', getmaxreleased boards')
    print (21, length boards21, getmaxreleased boards21)
    print (22, length boards22, getmaxreleased boards22)
    print (23, length boards23, getmaxreleased boards23)
    print (24, length boards24, getmaxreleased boards24)
    print (25, length boards25, getmaxreleased boards25)
    print (26, length boards26, getmaxreleased boards26)
    print (27, length boards27, getmaxreleased boards27)
    print (28, length boards28, getmaxreleased boards28)
    print (29, length boards29, getmaxreleased boards29)
    print (30, length boards30, getmaxreleased boards30)
