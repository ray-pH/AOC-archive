import System.IO 

getAdjacent :: (Int,Int) -> [String] -> [Char]
getAdjacent (row,col) seats =
    let height = length seats
        width  = length $ head seats
        btop = row - 1 >= 0
        bbot = row + 1 < height
        blef = col - 1 >= 0
        brig = col + 1 < width
        top = if btop then (seats !! (row-1) !! col) else '.'
        bot = if bbot then (seats !! (row+1) !! col) else '.'
        lef = if blef then (seats !! row !! (col-1)) else '.'
        rig = if brig then (seats !! row !! (col+1)) else '.'
        dtl = if btop && blef then (seats !! (row-1) !! (col-1)) else '.'
        dtr = if btop && brig then (seats !! (row-1) !! (col+1)) else '.'
        dbl = if bbot && blef then (seats !! (row+1) !! (col-1)) else '.'
        dbr = if bbot && brig then (seats !! (row+1) !! (col+1)) else '.'
     in [dtl,top,dtr,lef,rig,dbl,bot,dbr]

howm :: Eq a => a -> [a] -> Int
howm e = length . filter (== e)

-- proc state numb#
proc :: Char -> Int -> Char
proc '.' _ = '.'
proc 'L' n = if n == 0 then '#' else 'L' 
proc '#' n = if n <  4 then '#' else 'L'

iter :: [String] -> [String]
iter seats = [ [ f row col | col<-[0..wid-1] ] | row<-[0..hei-1] ]
    where hei = length seats
          wid = length $ head seats
          f row col = proc state num 
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
        height = length seats
        width  = length $ head seats
        stable = simul seats
        count = sum [ howm '#' row | row <- stable ]
    mapM_ print seats
    putStrLn "\niterate\n"
    mapM_ print stable
    print count
