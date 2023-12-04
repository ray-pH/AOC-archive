import System.IO
import Data.List
import Debug.Trace

type Player = (Int,Int) -- score pos
type Game = (Player,Player,Int,Int) -- p1 p2 count turn

-- total 3*3*3 = 27
--
-- (3) : 1
-- 1 1 1
-- (4) : 3
-- 1 1 2 #
-- (5) : 6
-- 1 1 3 #
-- 2 2 1 #
-- (6) : 7
-- 2 2 2
-- 1 2 3 ##
-- (7) : 6
-- 3 3 1 #
-- 2 2 3 #
-- (8) : 3
-- 3 3 2 #
-- (9) : 1
-- 3 3 3

mcMap = zip [3..] [1,3,6,7,6,3,1] :: [(Int,Int)] -- (move,count)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

parseInput :: [String] -> Game
parseInput = f . zip [0,0] . map ((read :: String -> Int). last . words)
    where f (p1:p2:[]) = (p1,p2,1,1)

shiftedMod :: Int -> Int -> Int
shiftedMod a b = 1 + mod (a-1) b

stepPlayer :: Player -> Int -> Player
stepPlayer (score,pos) m = (score + pos', pos')
    where pos' = shiftedMod (pos + m) 10

stepGame :: Game -> [Game]
stepGame (p1, p2, count, 1) = map fstep mcMap
    where fstep (m,c) = ((stepPlayer p1 m), p2, count*c, 2)
stepGame (p1, p2, count, 2) = map fstep mcMap
    where fstep (m,c) = (p1, (stepPlayer p2 m), count*c, 1)

concatGames :: [Game] -> [Game]
concatGames [] = []
concatGames [x] = [x]
concatGames (x:y:rs) 
    | p1x == p1y && p2x == p2y = concatGames (pcom:rs)
    | otherwise                = x : concatGames (y:rs)
    where (p1x,p2x,cx,tx) = x
          (p1y,p2y,cy,ty) = y
          pcom = (p1x, p2x, cx+cy, tx)

stepUniverse :: [Game] -> [Game]
stepUniverse = concatGames . sort . flatten . map stepGame

winScore = 21 :: Int

checkWin :: Game -> Bool
checkWin ((s1,_),(s2,_),_,_) = s1 >= winScore || s2 >= winScore

playUniverse :: [Game] -> [Game] -> ([Game],[Game]) -- (univ',udone)
playUniverse [] udone = ([],udone)
playUniverse univ udone = trace (show $ length ugooo') playUniverse ugooo' (udone ++ udone')
    where univ'  = stepUniverse univ
          udone' = [game | game <- univ', checkWin game]
          ugooo' = [game | game <- univ', not $ checkWin game]

toCount :: Game -> (Int,Int)
toCount ((s1,_),(s2,_),c,_)
    | s1 >= winScore = (c,0)
    | s2 >= winScore = (0,c)

addTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
addTuple (x,y) (a,b) = (x+a, y+b)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        game = parseInput lin
        univ = [game]
        (univ',udone) = playUniverse univ []
        scores = map toCount udone
        wins = foldr addTuple (0,0) scores
        result = max (fst wins) (snd wins)
    print univ
    print wins
    print result
