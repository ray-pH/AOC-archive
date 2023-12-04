import System.IO
import Debug.Trace

type Vec = (Int,Int)

minmax :: Ord a => [a] -> (a,a)
minmax arr = (minimum arr, maximum arr)

parseImage :: [String] -> [Vec]
parseImage imgStr = [(i,j)| i <- [0..(row-1)], j <- [0..(col-1)], '#' == imgStr !! i !! j ]
    where row = length imgStr
          col = length (head imgStr)

genCoords :: Vec -> Vec -> [Vec]
genCoords (xa,xb) (ya,yb) = [(i,j)| i <- [(xa-1)..(xb+1)], j<-[(ya-1)..(yb+1)]]

boolArrBin :: [Bool] -> Int
boolArrBin = sum . map (\x -> 2^x) . map fst . filter snd . zip [0..] . reverse

getBinary :: Bool -> [Vec] -> Vec -> Int
getBinary inv img (x,y) = boolArrBin binarr'
    where neighCoords = [(i,j)| i<-[(x-1)..(x+1)], j<-[(y-1)..(y+1)]]
          binarr = [elem c img | c <- neighCoords]
          binarr' = if inv then map not binarr else binarr

doStep :: String -> (Char,[Vec]) -> (Char,[Vec])
doStep algo (c,img) = (c', map fst $ filter ((==) c' . snd) zipped)
    where xrange = minmax $ map fst img
          yrange = minmax $ map snd img
          coords = genCoords xrange yrange
          binaries = map (getBinary (c=='.') img) coords
          algos = map (\x -> algo !! x) binaries
          zipped = zip coords algos
          flipc '#' = '.'
          flipc '.' = '#'
          -- for inpex, don't flip
          c' = flipc c
          -- c' = c

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ []  = []
chunkOf n arr = take n arr : chunkOf n (drop n arr)

showImg :: [Vec] -> [String]
showImg img = chunkOf len $ map f $ map (\c -> elem c img) coords
    where xrange = minmax $ map fst img
          yrange = minmax $ map snd img
          coords = genCoords xrange yrange
          len    = snd xrange - fst xrange + 3
          f True  = '#'
          f False = '.'

enhanceN :: Int -> String -> (Char,[Vec]) -> (Char,[Vec])
enhanceN 0 _ (c,img) = (c,img)
enhanceN n algo (c,img) = trace (show n ++ ": " ++ (show $ length img)) enhanceN (n-1) algo $ doStep algo (c,img)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        (algo:"":imgStr) = lin
        img = parseImage imgStr
        (c',img') = enhanceN 50 algo ('#',img)
    print $ length img'
