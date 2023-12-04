import System.IO


evalGame :: String -> Integer
evalGame "X" = 0
evalGame "Y" = 3
evalGame "Z" = 6

evalHand :: String -> String -> Integer
evalHand "A" "X" = 3
evalHand "B" "X" = 1
evalHand "C" "X" = 2
evalHand "A" "Y" = 1
evalHand "B" "Y" = 2
evalHand "C" "Y" = 3
evalHand "A" "Z" = 2
evalHand "B" "Z" = 3
evalHand "C" "Z" = 1

calcScore :: String -> Integer
calcScore str = (evalGame hand) + (evalHand opp hand)
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
