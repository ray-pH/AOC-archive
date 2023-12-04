import System.IO

type Vec = (Int,Int)
type State = (Vec,Vec) -- pos, vel
type Area = (Vec,Vec)

sign :: Int -> Int
sign 0 = 0
sign x
    | x > 0 = 1
    | otherwise = 01

stripNonNumeric :: String -> String
stripNonNumeric "" = ""
stripNonNumeric (c:cs) 
    | ('0' <= c && c <= '9') || c == '-' = c : stripNonNumeric cs
    | otherwise = ' ': stripNonNumeric cs

parseInput :: String -> Area
parseInput str = ((a,b),(c,d))
    where (a:b:c:d:_)  = map read $ words $ stripNonNumeric str

getPos :: State -> Vec
getPos (pos,_) = pos

step :: State -> State
step ((x,y), (vx,vy)) = ((x+vx,y+vy), (vx+dvx,vy-1))
    where dvx = - sign vx

inArea :: Area -> State -> Bool
inArea area (pos,_) = x1 <= x && x <= x2 && y1 <= y && y <= y2
    where (xrange,yrange) = area
          (x1,x2) = xrange
          (y1,y2) = yrange
          (x,y) = pos

beyondArea :: Area -> State -> Bool
beyondArea area (pos,vel) = vy < 0 && y < y1
    where (_,yrange) = area
          (y1,y2) = yrange
          (_,vy) = vel
          (_,y) = pos

simulate :: Area -> State -> (Maybe Int) -> (Maybe Int)
simulate area state (Just y)
    | beyondArea area state = Nothing
    | inArea area state = Just y
    | otherwise = simulate area (step state) (Just nexty)
    where ((_,curry),_) = state
          nexty = max y curry

-- Domain
-- vx -> first step x still in area
--       x' = 0 + vx0
--       x1 <= vx0 <= x2
--       
-- vy -> dy at area is not bigger than yrange of area
--       y = 0 + vy0 t - 1/2 t^2 = vy0 t - 1/2 t^2
--       1/2 t^2 - vy0 t + y2 = 0
--       t = (vy0 ± sqrt(vy0^2 - 2 y2))
--
--       vy = vy0 - t = -sqrt(vy0^2 - 2 y2))
--       |vy| < y2 - y1
--       y2 - y1 > sqrt(vy0^2 - 2 y2)
--       (y2 - y1)^2 > vy0^2 - 2 y2
--       vy0^2 < (y2 - y1)^2 + 2 y2
--       vy0 < sqrt((y2 - y1)^2 + 2 y2) < y2 - y1 + |2*y2|

genDomain :: Area -> [Vec]
genDomain ((x1,x2),(y1,y2)) = [(vx,vy)| vx <- [xa..xb], vy <- [ya..yb]]
    where ymax = y2 - y1 - y2-- since negative
          yrange = (-ymax,ymax)
          xa = min 0 x1
          xb = max 0 x2
          (ya,yb) = yrange

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        inp = lin !! 0
        area = parseInput inp
        domain = genDomain area
        initpos = (0,0)
        initvel = (6,8)
        initstate = (initpos,initvel)
        simulated = map (\v -> simulate area (initpos,v) (Just 0)) domain
        filtered = filter ((/=) Nothing) simulated
        values = map (\(Just x) -> x) filtered
        result = length values
    print area
    print $ head domain
    print $ last domain
    -- print domain
    print values
    print result
