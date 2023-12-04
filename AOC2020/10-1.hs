import System.IO
import Data.List

diff :: [Int] -> [Int]
diff (x:[]) = [3]
diff (x:xs) = (head xs - x) : diff xs

howm :: Eq a => a -> [a] -> Int
howm e = length . filter (== e)

main :: IO()
main = do
    file <- openFile "10-input.txt" ReadMode
    -- file <- openFile "10-ex2.txt" ReadMode
    cont <- hGetContents file
    let lns = lines cont
        adapters = sort $ map (read :: String -> Int) lns
        d = 1 : diff adapters
        [d1, d3] = map (\y-> howm y d) [1,3]
    print adapters
    print d
    print d1
    print d3
    print $ d1*d3
