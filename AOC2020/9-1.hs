import System.IO


subset :: Eq a => Int -> Int -> [a] -> [a]
subset start end arr = take (end - start) (drop start arr)

isPossib :: Int -> [Int] -> Bool
isPossib n list = or [elem (n-x) list | x<-list]

isValid :: Int -> Int -> [Int] -> Bool
isValid n pre list
      | n < pre = True
      | otherwise = isPossib (list !! n) sub 
    where sub = subset (n-pre) n list

main = do
    file <- openFile "9-input.txt" ReadMode
    -- file <- openFile "9-ex.txt" ReadMode
    contents <- hGetContents file
    let preamble = 25
    let datas = [ (read s)::Int | s <- lines contents ]
    let valid = map (\y -> isValid y preamble datas) [0..(length datas)-1]
    let err = [ x | (x,y) <- zip datas valid, not y ]
    print err
