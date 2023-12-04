import System.IO


evalHand :: String -> Integer
evalHand "X" = 1
evalHand "Y" = 2
evalHand "Z" = 3

evalGame :: String -> String -> Integer
evalGame "A" "X" = 3
evalGame "A" "Y" = 6
evalGame "A" "Z" = 0
evalGame "B" "X" = 0
evalGame "B" "Y" = 3
evalGame "B" "Z" = 6
evalGame "C" "X" = 6
evalGame "C" "Y" = 0
evalGame "C" "Z" = 3

calcScore :: String -> Integer
calcScore str = (evalHand hand) + (evalGame opp hand)
      where [opp,hand] = words str

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        scores = map calcScore lin
    -- print scores
    print $ sum scores
