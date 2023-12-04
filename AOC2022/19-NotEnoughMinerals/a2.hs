import System.IO
import Data.List(sortBy)
import Debug.Trace

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

type Material = [Int]
mOre  = 0 :: Int
mClay = 1 :: Int
mObs  = 2 :: Int
mGeo  = 3 :: Int
mmat  = [[1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1]] :: [[Int]]

addMat :: Material -> Material -> Material
addMat [o1,c1,s1,g1] [o2,c2,s2,g2] = [o1+o2, c1+c2, s1+s2, g1+g2]
subMat :: Material -> Material -> Material
subMat [o1,c1,s1,g1] [o2,c2,s2,g2] = [o1-o2, c1-c2, s1-s2, g1-g2]

allLEQ :: Material -> Material -> Bool
allLEQ [o1,c1,s1,g1] [o2,c2,s2,g2] = and [o1<=o2, c1<=c2, s1<=s2, g1<=g2]

isPositive :: Material -> Bool
isPositive = all ((<=) 0)

--               ore_c, clay_c, obs_c, geo_c
type Blueprint = [Material]

parseBlueprint :: String -> Blueprint
parseBlueprint str = [ore_ci,0,0,0] : [clay_ci,0,0,0] : [soi,sci,0,0] : [goi,0,gsi,0] : []
    where splitted = splitBy '.' str
          [os,cs,ss,gs,_] = splitted
          ore_ci = read $ flip (!!) 1 $ reverse $ words os :: Int
          clay_ci= read $ flip (!!) 1 $ reverse $ words cs :: Int
          [soi, sci] = map (read . (!!) (reverse $ words ss)) [4,1] :: [Int]
          [goi, gsi] = map (read . (!!) (reverse $ words gs)) [4,1] :: [Int]

--             BP        robot-count material
data Board = B Blueprint Material    Material
instance Show Board where
    show (B bp robot mat) = "B " ++ (show robot) ++ " " ++ (show mat)
getGeodeBoard (B _ _ [o,c,s,g]) = g
getObsBoard   (B _ _ [o,c,s,g]) = s
getMats  (B _ _ m) = m
getRobot (B _ r _) = r
getGeobot (B _ [o,c,s,g] _) = g

data Move     = Build Int | Idle deriving Show
genPossibleMove :: Board -> [Move]
genPossibleMove (B bp rob mat)
    | isPositive (subMat mat gc) = [Build mGeo]
    | otherwise                  = Idle : (bo_move ++ bc_move ++ bs_move)
    where [oc,cc,sc,gc] = bp
          bo_move = if isPositive (subMat mat oc) then [Build mOre ]  else []
          bc_move = if isPositive (subMat mat cc) then [Build mClay] else []
          bs_move = if isPositive (subMat mat sc) then [Build mObs ]  else []

collectOre :: Board -> Board
collectOre (B bp rob mat) = (B bp rob (addMat mat rob))

domove :: Move -> Board -> Board
domove Idle bo = bo
domove (Build mid) (B bp rob mat) = B bp (addMat rob (mmat !! mid)) (subMat mat matused)
  where matused = bp !! mid

stepBoard :: Board -> [Board]
stepBoard board  = map (flip domove board') moves
    where moves  = genPossibleMove board
          board' = collectOre board

compareBoard :: Board -> Board -> Ordering
compareBoard (B _ rob1 mat1) (B _ rob2 mat2) = compare (rob1++mat1) (rob2++mat2)

-- asuume already sorted
simplifyBoards :: [Board] -> [Board]
simplifyBoards [] = []
simplifyBoards (b:[]) = [b]
simplifyBoards (b1:b2:bs)
  | samerobot && lessmaterial = simplifyBoards (b2:bs)
  | otherwise                 = b1 : simplifyBoards (b2:bs)
  where samerobot    = (getRobot b1) == (getRobot b2)
        lessmaterial = allLEQ (getMats b1) (getMats b2)

-- clean low geodes
cleanLowGeodes :: [Board] -> [Board]
cleanLowGeodes boards = filter ((==) maxgeodes. getGeodeBoard) boards
    where maxgeodes = maximum $ map getGeodeBoard boards

-- clean low obs
cleanLowObs :: [Board] -> [Board]
cleanLowObs boards = filter ((==) maxobs. getObsBoard) boards
    where maxobs = maximum $ map getObsBoard boards

-- clean low geodebot
cleanLowGeodeBot :: [Board] -> [Board]
cleanLowGeodeBot boards = filter ((==) maxgeodes. getGeobot) boards
    where maxgeodes = maximum $ map getGeobot boards

stepBoards :: [Board] -> [Board]
stepBoards = simplifyBoards . sortBy compareBoard . concat . map stepBoard
-- stepBoards = cleanLowGeodes . simplifyBoards . sortBy compareBoard . concat . map stepBoard
-- stepBoards = cleanLowObs . cleanLowGeodeBot . cleanLowGeodes . simplifyBoards . sortBy compareBoard . concat . map stepBoard
-- stepBoards = cleanLowObs . cleanLowGeodeBot . simplifyBoards . sortBy compareBoard . concat . map stepBoard

-- stepBoards :: [Board] -> [Board]
-- stepBoards bo = trace (show $ length boo) boo
--     where boo = f bo
--           -- f = simplifyBoards . sortBy compareBoard . concat . map stepBoard
--           f = cleanLowGeodeBot . simplifyBoards . sortBy compareBoard . concat . map stepBoard

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        bps = map parseBlueprint lin
        target  = 24 :: Int
        --
        -- bp0 = bps !! 0
        -- board0 = B bp0 [1,0,0,0] [0,0,0,0]
        -- boards0 = [board0]
        -- boards  = take (target + 1) $ iterate stepBoards boards0
        -- finalboards = last boards
        --
        bps_board0 = map (\bp -> B bp [1,0,0,0] [0,0,0,0]) bps :: [Board]
        bps_boards0 = map (\x -> [x]) bps_board0 :: [[Board]]
        bps_boards  = map (take (target + 1) . iterate stepBoards) bps_boards0 :: [[[Board]]]
        bps_finalboards = map last bps_boards
        bps_maxgeo  = map (maximum . map getGeodeBoard) bps_finalboards :: [Int]
        qualitylevel = sum $ map (\(a,b) -> a*b) $ zip [1..] bps_maxgeo
        --
    mapM_ print $ zip [0..] $ map length $ bps_boards !! 0
    -- mapM_ print $ take 10 $ bps_finalboards !! 0
    mapM_ print $ bps_finalboards !! 0
    -- mapM_ print $ zip [1..] bps_maxgeo
    -- mapM_ print $ zip [1..] bps_maxgeo
    -- print qualitylevel
