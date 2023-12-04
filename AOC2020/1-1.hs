import System.IO

sumTo2020 :: [[Integer]] -> [Integer]
sumTo2020 [] = []
sumTo2020 xs = head [x | x<-xs, sum x == 2020]

rList :: [String] -> [Integer] 
rList = map read

main :: IO()
main = do
    handle <- openFile "1-input.txt" ReadMode
    contents <- hGetContents handle
    let lis = rList (words contents)
    let pair = [[x,y] | x<-lis, y<-lis, x<y]
    let res = sumTo2020 pair
    putStrLn (show(product res))
