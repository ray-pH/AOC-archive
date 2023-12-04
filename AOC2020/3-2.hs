import System.IO

count :: Eq a => a -> [a] -> Integer
count x arr = toInteger $ length $ filter (x==) arr

tree :: [String] -> (Integer,Integer) -> Integer
tree tmap (step,down) =
    let width = toInteger $ length $ head tmap
        ns = [0..(toInteger ((length tmap)-1))]
        zipped = tail [ (mod (step*(div x down)) width, m) | (x,m) <- (zip ns tmap), (mod x down) == 0 ]
        -- zipped = tail $ zip nmod tmap
        path = [y!!(fromInteger x) | (x,y)<-zipped]
    in (count '#' path)

main :: IO()
main = do
    file <- openFile "3-input.txt" ReadMode
    contents <- hGetContents file
    let tmap = lines contents
    let ones = [(1,1), (3,1) , (5,1), (7,1), (1,2)]
    let trees = map (\y -> tree tmap y) ones
    print trees
    print $ product trees
    -- mapM_ print zipped
    -- mapM_ print path
