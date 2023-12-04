import System.IO
import Data.List
import Data.Maybe

diff :: [Int] -> [Int]
diff (x:[]) = [3]
diff (x:xs) = (head xs - x) : diff xs

howm :: Eq a => a -> [a] -> Int
howm e = length . filter (== e)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

possib :: [Int] -> Int
possib arr 
  | length arr <= 1 = 1
  | length arr == 2 = 2
  | length arr == 3 = 4
  | length arr == 4 = 7
  | otherwise =1

main :: IO()
main = do
    file <- openFile "10-input.txt" ReadMode
    -- file <- openFile "10-ex2.txt" ReadMode
    cont <- hGetContents file
    let lns = lines cont
        adapters = sort $ map (read :: String -> Int) lns
        d = 1 : diff adapters
        [d1, d3] = map (\y-> howm y d) [1,3]
        spl = split 3 d
        ll = map length spl
        ps = map possib spl
    print adapters
    print d
    print spl
    print ll
    print ps
    print $ product ps
