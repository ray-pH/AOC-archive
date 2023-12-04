import System.IO
import Data.List

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr


main :: IO()
main = do
    -- handle   <- openFile "inpex.txt" ReadMode
    handle   <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let l = lines contents
        grouped = splitBy "" l
        summed  = map (sum . map read) grouped
        sorted  = reverse $ sort summed
        top3    = take 3 sorted
    print $ sum top3
