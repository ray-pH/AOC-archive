import System.IO 
import Data.List

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

type Player = [Int]

getPlayers :: [String] -> (Player,Player)
getPlayers rawlines = let [line1, line2] = split "" rawlines
                          player1 = map read $ tail line1
                          player2 = map read $ tail line2
                       in (player1, player2)

playRound :: (Player,Player) -> (Player,Player)
playRound (p1,p2) = let (c1 : p1')   = p1
                        (c2 : p2')   = p2 
                        toapp        = reverse $ sort [c1,c2]
                        (p1'', p2'') = if c1 > c2
                                  then (p1'++toapp, p2')
                                  else (p1', p2'++toapp)
                     in (p1'',p2'') 

type Winner = Player
playGame :: Player -> Player -> Winner
playGame [] p2 = p2
playGame p1 [] = p1
playGame p1 p2 = let (p1',p2') = playRound (p1,p2) 
                  in playGame p1' p2'

calcScore :: Winner -> Int
calcScore winner = sum [ v*n | (v,n) <- zip (reverse winner) [1..] ] 

main :: IO()
main = do
    file <- openFile "22-input.txt" ReadMode
    -- file <- openFile "22-ex.txt" ReadMode
    cont <- hGetContents file
    let line    = lines cont
        (p1,p2) = getPlayers line
        winner  = playGame p1 p2
        score   = calcScore winner
    print p1
    print p2
    print winner
    print score
