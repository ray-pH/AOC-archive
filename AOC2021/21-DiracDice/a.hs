import System.IO

type Player = (Int,Int) -- score pos
type Game = (Player,Player,Int,Int) -- p1 p2 dice turn

parseInput :: [String] -> Game
parseInput = f . zip [0,0] . map ((read :: String -> Int). last . words)
    where f (p1:p2:[]) = (p1,p2,1,1)

shiftedMod :: Int -> Int -> Int
shiftedMod a b = 1 + mod (a-1) b

step :: Game -> Game
step ((s1,p1), pl2 ,dice, 1) = ((s1+p1',p1'), pl2, shiftedMod (dice+3) 100, 2)
    where move = sum [ shiftedMod (dice+d) 100 | d <- [0,1,2] ]
          p1' = shiftedMod (p1+move) 10
step (pl1, (s2,p2) ,dice, 2) = (pl1, (s2+p2',p2'), shiftedMod (dice+3) 100, 1)
    where move = sum [ shiftedMod (dice+d) 100 | d <- [0,1,2] ]
          p2' = shiftedMod (p2+move) 10

checkWin :: Game -> Bool
checkWin ((s1,_),(s2,_),_,_) = s1 >= 1000 || s2 >= 1000

getLoser :: Game -> Int
getLoser ((s1,_),(s2,_),_,_)
    | s1 >= 1000 = s2
    | s2 >= 1000 = s1

playGame :: Int -> Game -> Int
playGame turn game
    | checkWin game = turn * 3 * getLoser game
    | otherwise     = playGame (turn+1) $ step game

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        game = parseInput lin
        result = playGame 0 game
    -- print game
    print result
