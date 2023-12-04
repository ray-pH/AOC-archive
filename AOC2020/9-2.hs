import System.IO
import Data.List
import Data.Maybe

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
        datas = [ (read s)::Int | s <- lines contents ]
        valid = map (\y -> isValid y preamble datas) [0..(length datas)-1]
        err = [ x | (x,y) <- zip datas valid, not y ]
        target = head err
        ls = [2..length datas]
        subs = [[ subset (x-l) x datas | x <- [l..length datas] ] | l<-ls]
        sums = [map (foldr (+) 0) sub | sub <- subs]
        exists = [elem target arr | arr <- sums]
        id = fromJust $ elemIndex True exists
        idd = fromJust $ elemIndex target (sums !! id)
        thesubs = subs !! id !! idd
    print $ minimum thesubs + maximum thesubs
