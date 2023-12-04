import System.IO

type Pos = (Int, Int, Int)

move :: Pos -> [String] -> Pos
move pos [] = pos
move (x,y,z) (r:rs)
    | dir == "forward"  = move (x+d,y+z*d,z) rs
    | dir == "up"       = move (x,y,z-d) rs
    | dir == "down"     = move (x,y,z+d) rs
    where [dir, dStr] = words r
          d = read dStr :: Int

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
    print $ (\(x,y,_) -> x*y) $ move (0,0,0) lin
