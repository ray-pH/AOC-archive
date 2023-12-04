import System.IO

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==) 

main = do
    file <- openFile "3-input.txt" ReadMode
    contents <- hGetContents file
    let tmap = lines contents
    let width = length $ head tmap
    let ns = [0..((length tmap)-1)]
    let nmod = [ mod (3*x) width | x <- ns] 
    let zipped = tail $ zip nmod tmap
    let path = [y!!x | (x,y)<-zipped ]
    print (count '#' path)
    -- mapM_ print zipped
    -- mapM_ print path
