import System.IO

data Rule = Rule { name :: String
                 , count :: [Int] 
                 , item :: [String]
                 } deriving Show

toItem :: [String] -> [String]
toItem [] = []
toItem (a:b:c:d:xs) = (a++" "++b++" "++c) : toItem xs

toRule :: [String] -> Rule
-- toRule (a:b:_:_:"no":"other":xs) = Rule nm [1] [nm]
toRule (a:b:_:_:"no":"other":xs) = Rule nm [] []
    where nm = (a++" "++b)
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

getRule :: String -> [Rule] -> Rule
getRule nm rls = head [r | r<-rls, (name r) == nm]

append :: ([String],[Int]) -> ([String],[Int]) -> ([String],[Int])
append (s1,i1) (s2,i2) = (s1++s2, i1++i2)

expandItems :: ([String],[Int]) -> [Rule] -> ([String],[Int])
expandItems (nms,cts) rl =
    let z = zip nms cts
        arr = [ ( map (* ct) (count r), item r) | (nm,ct) <- z, let r = getRule nm rl] 
        cts' = foldr (++) [] [ c | (c,_) <- arr ]
        nms' = foldr (++) [] [ n | (_,n) <- arr ]
     in if (nms',cts') == (nms, cts)
           then (nms, cts)
           else append ((nms,cts)) (expandItems (nms',cts') rl)

main :: IO()
main = do
    file <- openFile "7-input.txt" ReadMode
    -- file <- openFile "7-ex2.txt" ReadMode
    cont <- hGetContents file
    let rules = [toRule $ words line | line <- (lines cont)]
    let r = getRule "shiny gold" rules
    let arr = expandItems (item r, count r) rules
    let (nms,cts) = arr
    print $ zip nms cts
    print $ sum cts
