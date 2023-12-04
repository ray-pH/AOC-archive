import System.IO
import Data.List

type Vec = (Int,Int,Int)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

isNumCommaDash :: Char -> Bool
isNumCommaDash '-' = True
isNumCommaDash ',' = True
isNumCommaDash c   = '0' <= c && c <= '9'

toVec :: [Int] -> Vec
toVec (x:y:z:_) = (x,y,z)

parsePos :: String -> Vec
parsePos = toVec . map read . drop 2 . splitBy ',' . filter isNumCommaDash . drop 5

manhDist :: Vec -> Vec -> Int
manhDist (x,y,z) (x',y',z') = dx + dy + dz
    where dx = abs $ x' - x
          dy = abs $ y' - y
          dz = abs $ z' - z


main :: IO()
main = do
    -- file <- openFile "aresex.txt" ReadMode
    file <- openFile "ares.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        pos = (0,0,0):[parsePos x | (i,x) <- zip [0..] lin,  mod i 2 == 0]
        dists = [ manhDist a b | (ia,a) <- zip [0..] pos, (ib,b) <- zip [0..] pos, ia > ib ]
        res = last $ sort dists
    print pos
    print res
