import System.IO

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

bintodec :: Char -> String -> Int
bintodec c bin = 
    let n = reverse [0..((length bin) - 1)]
    in sum [ 2^y | (x,y) <- zip bin n, x == c]

row :: String -> Int
row st = bintodec 'B' ( substring 0 7 st )

col :: String -> Int
col st = bintodec 'R' ( substring 7 10 st )

seat :: String -> Int
seat st = 8 * (row st) + (col st)

main :: IO()
main = do
    file <- openFile "5-input.txt" ReadMode
    cont <- hGetContents file
    let line = lines cont
    let seats = map seat line
    print $ maximum seats
    -- mapM_ print seats
