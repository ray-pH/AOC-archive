import System.IO
import Data.List

type Pos  = (Int, Int)
type Path = [Pos]
type Grid = [[Char]]

--                   grid boundary  activesand counter
data Canvas = Canvas Grid (Pos,Pos) Pos        Int     deriving Show
getGrid    (Canvas grid _ _ _)  = grid
getSandPos (Canvas _ _ sandp _) = sandp
getCounter (Canvas _ _ _ c)     = c

sandsource = (500,0) :: Pos

offsetIdCanvas :: Canvas -> Pos -> Pos
offsetIdCanvas (Canvas _ ((x0,y0),_) _ _) (x,y) = (x-x0, y-y0)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

iterN :: Int -> (a->a) -> a -> a
iterN 0 _ val = val
iterN n f val = iterN (n-1) f (f val)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt id val arr = l ++ [val] ++ r
    where (l, (x:r)) = splitAt id arr

parsePath :: String -> Path
parsePath = map (\[x,y] -> (read x, read y)) . map (splitBy ',') .filter ((/=) "->") . words

genBetween :: Pos -> Pos -> [Pos]
genBetween (x1,y1) (x2,y2)
    | x1 == x2 = [(x1,y) | y <- [(ymin+1)..(ymax-1)] ]
    | y1 == y2 = [(x,y1) | x <- [(xmin+1)..(xmax-1)] ]
    where [ymin, ymax] = sort [y1, y2]
          [xmin, xmax] = sort [x1, x2]

fillFromPath :: Path -> [Pos]
fillFromPath (p:[])   = [p]
fillFromPath (p:q:ps) = [p] ++ (genBetween p q) ++ fillFromPath (q:ps)

getxminmax :: [Pos] -> (Int,Int)
getxminmax = (\x -> (head x, last x)) . sort . map fst . (:) sandsource
getyminmax :: [Pos] -> (Int,Int)
getyminmax = (\y -> (head y, last y)) . sort . map snd . (:) sandsource

genEmptyGrid :: (Pos,Pos) -> Grid
genEmptyGrid ((left,bot),(right,top)) = map (const row) [1..height]
    where width  = right-left +1
          height = top-bot +1
          row    = map (const '.') [1..width]

-- data Canvas = Canvas Grid (Pos,Pos) Pos
setCanvasValue :: Canvas -> Char -> Pos -> Canvas
setCanvasValue (Canvas grid (loP,hiP) activesand counter) c (x,y) = (Canvas grid' (loP,hiP) activesand counter)
    where canvas  = (Canvas grid (loP,hiP) activesand counter)
          (x',y') = offsetIdCanvas canvas (x,y)
          row     = grid !! y'
          row'    = replaceAt x' c row
          grid'   = replaceAt y' row' grid

fgenCanvas :: [Pos] -> Canvas -> Canvas
fgenCanvas [] canvas = canvas
fgenCanvas (p:ps) canvas = fgenCanvas ps $ setCanvasValue canvas '#' p

genCanvas :: [Path] -> Canvas
genCanvas paths = fgenCanvas filledPath canvas0
    where filledPath = flatten $ map fillFromPath paths :: [Pos]
          (x0,x1)    = getxminmax filledPath
          (y0,y1)    = getyminmax filledPath
          grid0      = genEmptyGrid ((x0,y0),(x1,y1))
          canvas0    = Canvas grid0 ((x0,y0),(x1,y1)) sandsource 0

stepSand :: Canvas -> Canvas
stepSand (Canvas grid boundary (-1,-1) c) = (Canvas grid boundary (-1,-1) c)
stepSand (Canvas grid boundary (xsand,ysand) c)
    | d_isout      = (Canvas grid boundary (-1,-1) c)
    | cdown == '.' = (Canvas grid boundary (xsand,ysand+1) c)
    | ld_isout     = (Canvas grid boundary (-1,-1) c)
    | cld   == '.' = (Canvas grid boundary (xsand-1,ysand+1) c)
    | rd_isout     = (Canvas grid boundary (-1,-1) c)
    | crd   == '.' = (Canvas grid boundary (xsand+1,ysand+1) c)
    | otherwise    = (Canvas grid boundary (xsand,ysand) c)
    where canvas = (Canvas grid boundary (xsand,ysand) c)
          (xdown',ydown') = offsetIdCanvas canvas (xsand, ysand+1)
          d_isout         = ydown' >= length grid
          cdown           = (grid !! ydown') !! xdown'
          (xld',yld')     = offsetIdCanvas canvas (xsand-1, ysand+1)
          ld_isout        = xld' < 0
          cld             = (grid !! yld') !! xld'
          (xrd',yrd')     = offsetIdCanvas canvas (xsand+1, ysand+1)
          rd_isout        = xrd' >= (length $ grid !! 0)
          crd             = (grid !! yrd') !! xrd'

turnSand :: Canvas -> Canvas
turnSand canvas
    | (getSandPos canvas == getSandPos canvas') = canvas
    | otherwise   = turnSand canvas'
    where canvas' = stepSand canvas

resetSandPos :: Canvas -> Canvas
resetSandPos (Canvas grid boundary sandpos counter) = Canvas grid boundary sandsource (counter+1)

applySand :: Canvas -> Canvas
applySand canvas
    | getSandPos canvas == (-1,-1) = canvas
    | otherwise = resetSandPos $ setCanvasValue canvas 'o' sandpos
    where sandpos = getSandPos canvas

updateSand :: Canvas -> Canvas
updateSand = applySand . turnSand

runSimulation :: Canvas -> Canvas
runSimulation cv
  | getSandPos cv == (-1,-1) = cv
  | otherwise                = runSimulation $ updateSand cv

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin   = lines cont
        paths = map parsePath lin
        canvas = genCanvas paths
        canvas' = runSimulation canvas
    mapM_ print paths
    print canvas
    print canvas'
    mapM_ print $ getGrid canvas'
