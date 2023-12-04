import System.IO

data Rule = Rule { name :: String
                 , count :: [Int] 
                 , item :: [String]
                 } deriving Show

-- light red bags contain 1 bright white bag, 2 muted yellow bags.
-- 1 bright white bag, 2 muted yellow bags.

genUnique :: Eq a => [a] -> [a]
genUnique [] = []
genUnique (x:xs)
    | elem x xs = genUnique xs
    | otherwise = x : genUnique xs

toItem :: [String] -> [String]
toItem [] = []
toItem (a:b:c:d:xs) = (a++" "++b++" "++c) : toItem xs

toRule :: [String] -> Rule
toRule (a:b:_:_:"no":"other":xs) = Rule (a++" "++b) [] []
toRule (a:b:"bags":"contain":xs) =
    let items  = toItem xs
        itemss = [words i | i<-items]
        count  = [read $ head i | i <- itemss]
        tails  = [tail i | i <- itemss]
        item   = [x++" "++y | [x,y] <- tails]
     in Rule (a++" "++b) count item


isContBy :: String -> Rule -> Bool
isContBy name rule = elem name (item rule)

getContainer :: String -> [Rule] -> [String]
getContainer nm rl =
    let direct = [name r | r <- filter (isContBy nm) rl]
     in (foldr (++) direct) [getContainer x rl | x <- direct]

main :: IO()
main = do
    file <- openFile "7-input.txt" ReadMode
    -- file <- openFile "7-ex.txt" ReadMode
    cont <- hGetContents file
    let rules = [toRule $ words line | line <- (lines cont)]
    -- let res = filter (isContBy "shiny gold") rules
    let res = genUnique $ getContainer "shiny gold" rules
    mapM_ print res
    print $ length res
