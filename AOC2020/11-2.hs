import System.IO 

getTop :: (Int,Int) -> [String] -> Char
getTop (row,col) seats = if see == '-' then '.' else see 
    where arr = ([ seats !! r !! col | r <- reverse [0..row-1] ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getBot :: (Int,Int) -> [String] -> Char
getBot (row,col) seats = if see == '-' then '.' else see 
    where height = length seats
          arr = ([ seats !! r !! col | r <- [row+1..height-1] ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getLef :: (Int,Int) -> [String] -> Char
getLef (row,col) seats = if see == '-' then '.' else see 
    where arr = ([ seats !! row !! c | c <- reverse [0..col-1] ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getRig :: (Int,Int) -> [String] -> Char
getRig (row,col) seats = if see == '-' then '.' else see 
    where width = length $ head seats
          arr = ([ seats !! row !! c | c <- [col+1..width-1] ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getDtl :: (Int,Int) -> [String] -> Char
getDtl (row,col) seats = if see == '-' then '.' else see
    where dom = zip (reverse [0..row-1]) (reverse [0..col-1])
          arr = ([ seats !! r !! c | (r,c) <- dom ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getDtr :: (Int,Int) -> [String] -> Char
getDtr (row,col) seats = if see == '-' then '.' else see
    where width = length $ head seats
          dom = zip (reverse [0..row-1]) [col+1..width-1]
          arr = ([ seats !! r !! c | (r,c) <- dom ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getDbl :: (Int,Int) -> [String] -> Char
getDbl (row,col) seats = if see == '-' then '.' else see
    where height = length seats
          dom = zip [row+1..height-1] (reverse [0..col-1])
          arr = ([ seats !! r !! c | (r,c) <- dom ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getDbr :: (Int,Int) -> [String] -> Char
getDbr (row,col) seats = if see == '-' then '.' else see
    where height = length seats
          width = length $ head seats
          dom = zip [row+1..height-1] [col+1..width-1] 
          arr = ([ seats !! r !! c | (r,c) <- dom ] ++ ['-'])
          see = head $ dropWhile (== '.') arr

getAdjacent :: (Int,Int) -> [String] -> [Char]
getAdjacent (row,col) seats = [ g (row,col) seats | g <- f ]
    where f = [getDtl, getTop, getDtr, getLef, getRig, getDbl, getBot, getDbr]

howm :: Eq a => a -> [a] -> Int
howm e = length . filter (== e)

-- proc state numb#
proc :: Char -> Int -> Char
proc '.' _ = '.'
proc 'L' n = if n == 0 then '#' else 'L' 
proc '#' n = if n <  5 then '#' else 'L'

iter :: [String] -> [String]
iter seats = [ [ f row col | col<-[0..wid-1] ] | row<-[0..hei-1] ]
    where hei = length seats
          wid = length $ head seats
          f row col = proc state num 
              where state = seats !! row !! col
                    num = howm '#' (getAdjacent (row, col) seats)

itern :: [String] -> [[Int]]
itern seats = [ [ f row col | col<-[0..wid-1] ] | row<-[0..hei-1] ]
    where hei = length seats
          wid = length $ head seats
          f row col = num 
              where state = seats !! row !! col
                    num = howm '#' (getAdjacent (row, col) seats)

simul :: [String] -> [String]
simul seats = if seats' == seats then seats else simul seats'
    where seats' = iter seats

main :: IO()
main = do
    file <- openFile "11-input.txt" ReadMode
    -- file <- openFile "11-ex.txt" ReadMode
    cont <- hGetContents file
    let seats = lines cont
        -- s1 = iter seats
        stable = simul seats
        count = sum [ howm '#' row | row <- stable ]
    -- mapM_ print s1
    -- print $ getAdjacent (1,0) s1
    -- mapM_ print $ itern s1
    mapM_ print seats
    putStrLn "\niterate\n"
    -- mapM_ print $ iter seats
    -- mapM_ print $ iter s1
    mapM_ print stable
    print count
