import System.IO
import Data.List(splitAt)

type Pos  = (Int,Int)
type Rock = [Pos]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt id val arr = l ++ [val] ++ r
    where (l, (x:r)) = splitAt id arr

type Arr2D a = [[a]]

--        width(x) height(y) initVal
a2Dgen :: Int ->   Int ->    a ->    Arr2D a
a2Dgen  w h v = map (const row) [1..h]
    where row = map (const v) [1..w]

a2Dload :: Pos -> Arr2D a -> a
a2Dload (x,y) arr = (arr !! y) !! x

a2Dstore :: a -> Pos -> Arr2D a -> Arr2D a
a2Dstore val (x,y) arr = replaceAt y row'  arr
    where row  = arr !! y
          row' = replaceAt x val row

a2Dstorerow :: [a] -> Int -> Arr2D a -> Arr2D a
a2Dstorerow newrow y arr = replaceAt y newrow arr

-- ####  .#.  ..#   #  ##
--       ###  ..#   #  ##
--       .#.  ###   #
--                  #
r1 = [(0,0),(1,0),(2,0),(3,0)]            -- 1
r2 = [(1,0),  (0,1),(1,1),(2,1),  (1,2)]  -- 3
r3 = [(0,0),(1,0),(2,0),  (2,1),  (2,2)]  -- 3
r4 = [(0,0),(0,1),(0,2),(0,3)]            -- 4
r5 = [(0,0),(1,0),  (0,1),(1,1)]          -- 2

--             field        topy activerock   rocklist movlist rockcounter movementcounter
data Board = B (Arr2D Char) Int  (Maybe Rock) [Rock]   [Char]  Int         Int
getActiveRock (B _ _ ar _ _ _ _) = ar
getField      (B f _ _  _ _ _ _) = f
getMc         (B _ _ _  _ _ _ m) = m
getLowField   = take 10 . getField

getTopLayer (B f y _ _ _ _ _) = f !! y

instance Show Board where
    show (B field topy arock rs ms counter mc) = show (counter, mc, topy, arock)

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

genFloor :: Arr2D Char -> Arr2D Char
genFloor arr = a2Dstorerow floorrow 0 arr
    where width    = length $ head arr
          floorrow = map (const '#') [1..width]

translatePos :: Pos -> Pos -> Pos
translatePos (a,b) (m,n) = (a+m, b+n)

-- Each rock appears so that its left edge is two units away from the left wall and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
initRock :: Board -> Board
initRock (B field topy Nothing (r:rs) movlist counter mc) = (B field topy (Just newrock) rs movlist counter mc)
    where newrock = map (translatePos (2, topy  + 4)) r

storeRock :: Board -> Board
storeRock (B field topy (Just rock) rs ms counter mc) = B field' topy' Nothing rs ms (counter+1) mc
    where strockatpos ::Pos -> Arr2D Char -> Arr2D Char
          strockatpos pos field = a2Dstore '#' pos field
          field' = foldr strockatpos field rock
          topy'  = max topy (maximum $ map snd rock)
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--                        (P -> Ar -> Ar) -> Ar -> [P] -> Ar
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

tryDrop :: Board -> Board
tryDrop (B field topy (Just rock) rs ms counter mc)
    | blocked   = storeRock (B field topy (Just rock) rs ms counter mc)
    | otherwise = B field topy  (Just downrock) rs ms counter mc
    where downrock = map (translatePos (0,-1)) rock :: [Pos]
          blocked  = any ((==) '#' . flip a2Dload field) downrock

tryPush :: Board -> Board
tryPush (B field topy (Just rock) rs ms counter mc)
    | isoutside || blockedrock = (B field topy (Just rock)      rs ms counter mc')
    | otherwise                = (B field topy (Just transrock) rs ms counter mc')
    where m = ms !! mc
          mc' = mod (mc+1) (length ms)
          ftrans '>' = (1,0)
          ftrans '<' = (-1,0)
          transrock  = map (translatePos (ftrans m)) rock :: [Pos]
          blockedrock= any ((==) '#' . flip a2Dload field) transrock
          minx       = minimum $ map fst transrock
          maxx       = maximum $ map fst transrock
          isoutside  = minx < 0 || maxx >= (length $ head field)

stepRock :: Board -> Board
stepRock = tryDrop . tryPush

oneRock :: Board -> Board
oneRock b
    | isNothing $ getActiveRock b' = initRock b'
    | otherwise = oneRock b'
    where b' = stepRock b


main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin    = lines cont
        movls  = head lin
        rockls = cycle [r1,r2,r3,r4,r5] :: [Rock]
        field  = genFloor $ a2Dgen 7 (4*2020) '.' :: Arr2D Char
        board  = B field 0 Nothing rockls movls 0 0 :: Board
        board0 = initRock board
        boarditer = iterate oneRock board0
        -- toshow (B field topy arocks rs ms counter) = (counter, topy, arocks, field !! topy)
        toshow (B field topy arocks rs ms counter mc) = (counter, mc, topy, field !! topy)
        boardn n = boarditer !! n
        toconsider = take 16000 $ map toshow boarditer
        -- with11mc   = filter ((==) 11 . (\(c,mc,topy,tf) -> mc)) toconsider
        with12mc   = filter ((==) 12 . (\(c,mc,topy,tf) -> mc)) toconsider
    -- print $ length movls
    -- mapM_ print $ take 200 $ map toshow boarditer
    -- mapM_ print $ take 4023 $ map toshow boarditer
    mapM_ print $ take (1704+796+2) $ map toshow boarditer
    -- mapM_ print with12mc
    -- mapM_ print $ take 20 $ map toshow boarditer
    -- mapM_ print $ reverse $ getLowField $ boardn 9
