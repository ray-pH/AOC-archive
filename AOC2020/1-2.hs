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
    let triplet = [[x,y,z] | x<-lis, y<-lis, z<-lis, x<y, y<z]
    let res = sumTo2020 triplet
    putStrLn (show(product res))
