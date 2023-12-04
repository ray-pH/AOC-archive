import System.IO

type Pos = (Int, Int)

move :: Pos -> [String] -> Pos
move pos [] = pos
move (x,y) (r:rs)
    | dir == "forward"  = move (x+d,y) rs
    | dir == "up"       = move (x,y-d) rs
    | dir == "down"     = move (x,y+d) rs
    where [dir, dStr] = words r
          d = read dStr :: Int

main :: IO()
main = do
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
    print $ (\(x,y) -> x*y) $ move (0,0) lin
