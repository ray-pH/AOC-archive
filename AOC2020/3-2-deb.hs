
import System.IO

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==) 

main = do
    file <- openFile "3-ex.txt" ReadMode
    contents <- hGetContents file
    let tmap = lines contents
    let width = length $ head tmap
    let ns = [0..((length tmap)-1)]
    let zipped = tail [ (div (mod (1*x) width) 2, m) | (x,m) <- (zip ns tmap), mod x 2 == 0] 
    -- let zipped = tail $ zip nmod tmap
    let path = [y!!x | (x,y)<-zipped ]
    -- print (count '#' path)
    mapM_ print zipped
    print path
