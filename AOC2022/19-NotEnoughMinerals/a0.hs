import System.IO

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

--                ore clay obsidian geodes
data Material = M Int Int  Int      Int    deriving Show
mOre  = M 1 0 0 0
mClay = M 0 1 0 0
mObs  = M 0 0 1 0
mGeo  = M 0 0 0 1

instance Num Material where
    (+) (M o1 c1 s1 g1) (M o2 c2 s2 g2) = M (o1+o2) (c1+c2) (s1+s2) (g1+g2)
    (*) (M o1 c1 s1 g1) (M o2 c2 s2 g2) = M (o1*o2) (c1*c2) (s1*s2) (s1+s2)
    fromInteger x = M (fromIntegral x) (fromIntegral x) (fromIntegral x) (fromIntegral x)
    abs    = applyMatfun abs
    signum = applyMatfun signum
    negate = applyMatfun negate
applyMatfun :: (Int->Int) -> Material -> Material
applyMatfun f (M o c s g) = M (f o) (f c) (f s) (f g)

isPositive :: Material -> Bool
isPositive (M o c s g) = all ((>) 0) [o,c,s,g]

--                  ore_c    clay_c   obs_c    geo_c
data Blueprint = BP Material Material Material Material deriving Show

parseBlueprint :: String -> Blueprint
parseBlueprint str = BP (M ore_ci 0 0 0) (M clay_ci 0 0 0) (M soi sci 0 0) (M goi 0 gsi 0)
    where splitted = splitBy '.' str
          [os,cs,ss,gs,_] = splitted
          ore_ci = read $ flip (!!) 1 $ reverse $ words os :: Int
          clay_ci= read $ flip (!!) 1 $ reverse $ words cs :: Int
          [soi, sci] = map (read . (!!) (reverse $ words ss)) [4,1] :: [Int]
          [goi, gsi] = map (read . (!!) (reverse $ words gs)) [4,1] :: [Int]

--             BP        robot-count material
data Board = B Blueprint Material    Material
getGeodeBoard (B _ _ (M o c s g)) = g

data Move     = Build Material | Idle
genPossibleMove (B bp rob mat) = Idle : (bo_move ++ bc_move ++ bs_move ++ bg_move)
    where (BP oc cc sc gc) = bp
          bo_move = if isPositive (mat - oc) then [Build mOre ]  else []
          bc_move = if isPositive (mat - cc) then [Build mClay] else []
          bs_move = if isPositive (mat - sc) then [Build mObs ]  else []
          bg_move = if isPositive (mat - gc) then [Build mGeo ]  else []

collectOre :: Board -> Board
collectOre (B bp rob mat) = (B bp rob (mat + rob))

domove :: Move -> Board -> Board
domove Idle bo = bo
domove (Build mrobot) (B bp rob mat)
  | mrobot == mOre = 
  where (BP oc cc sc gc) = bp


main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        bps = map parseBlueprint lin
    -- print lin
    print bps
