import System.IO 
import Data.List
import Debug.Trace

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
                        p1have       = c1 <= length p1'
                        p2have       = c2 <= length p2'
                        (winner,wid) = if p1have && p2have 
                                          then playGame [] (take c1 p1') (take c2 p2')
                                          else case c1 > c2 of
                                                 True  -> (p1,1) 
                                                 False -> (p2,2) 
                     in case wid of
                          1 -> (p1'++[c1,c2], p2')
                          2 -> (p1', p2'++[c2,c1])

type Winner = Player

playGame :: [(Player,Player)] -> Player -> Player -> (Winner,Int)
playGame _    [] p2 = (p2,2)
playGame _    p1 [] = (p1,1)
-- playGame hist p1 p2 = let (p1',p2') = trace (show (p1,p2)) $ playRound (p1,p2) 
playGame hist p1 p2 = let (p1',p2') = playRound (p1,p2) 
                  in if elem (p1',p2') hist 
                        then (p1',1)
                        else playGame ((p1,p2):hist) p1' p2'

calcScore :: Winner -> Int
calcScore winner = sum [ v*n | (v,n) <- zip (reverse winner) [1..] ] 

main :: IO()
main = do
    file <- openFile "22-input.txt" ReadMode
    -- file <- openFile "22-ex.txt" ReadMode
    cont <- hGetContents file
    let line    = lines cont
        (p1,p2) = getPlayers line
        (winner,_)  = playGame [] p1 p2
        score   = calcScore winner
    print p1
    print p2
    print winner
    print score
