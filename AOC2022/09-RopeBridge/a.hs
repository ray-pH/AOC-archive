import System.IO

type Pos  = (Int,Int)
type Rope = (Pos,Pos)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

parseMovement :: [String] -> [(Char,Int)]
parseMovement [] = []
parseMovement (x:xs) = (head l, read r) : parseMovement xs
    where [l,r] = words x

expandMovement :: [(Char,Int)] -> [Char]
expandMovement [] = []
expandMovement ((c,1):xs) = c : expandMovement xs
expandMovement ((c,n):xs) = c : expandMovement ((c,n-1):xs)

updateTail :: Rope -> Rope
updateTail ((hx, hy), (tx, ty))
  | hx == tx && hy - ty > 1  = ((hx,hy), (tx, ty+1))
  | hx == tx && hy - ty < -1 = ((hx,hy), (tx, ty-1))
  | hy == ty && hx - tx > 1  = ((hx,hy), (tx+1, ty))
  | hy == ty && hx - tx < -1 = ((hx,hy), (tx-1, ty))
  | isConnected = ((hx,hy), (tx,ty))
  | otherwise  = diagonalxTail $ diagonalyTail ((hx,hy), (tx,ty))
  where isConnected = and [abs (hx-tx) <= 1, abs (hy-ty) <= 1]
        diagonalxTail :: Rope -> Rope
        diagonalxTail ((hxx,hyy), (txx,tyy))
          | hxx - txx > 0 = ((hxx,hyy), (txx+1, tyy))
          | hxx - txx < 0 = ((hxx,hyy), (txx-1, tyy))
        diagonalyTail :: Rope -> Rope
        diagonalyTail ((hxx,hyy), (txx,tyy))
          | hyy - tyy > 0 = ((hxx,hyy), (txx, tyy+1))
          | hyy - tyy < 0 = ((hxx,hyy), (txx, tyy-1))

moveRope :: Rope -> Char -> Rope
moveRope ((hx,hy), (tx,ty)) 'U' = updateTail ((hx,hy+1), (tx,ty))
moveRope ((hx,hy), (tx,ty)) 'D' = updateTail ((hx,hy-1), (tx,ty))
moveRope ((hx,hy), (tx,ty)) 'R' = updateTail ((hx+1,hy), (tx,ty))
moveRope ((hx,hy), (tx,ty)) 'L' = updateTail ((hx-1,hy), (tx,ty))

moveRopeWithHistory :: [Rope] -> [Char] -> [Rope]
moveRopeWithHistory history [] = history
moveRopeWithHistory history (c:cs) = moveRopeWithHistory (history ++ [newrope]) cs
    where currentrope = last history
          newrope     = moveRope currentrope c

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        movements = parseMovement lin
        movements' = expandMovement movements
        initrope  = ((0,0),(0,0)) :: Rope
        hists     = moveRopeWithHistory [initrope] movements'
        tailhist  = map (\(_,tail) -> tail) hists
        tailvisited = unique tailhist
    print movements'
    mapM_ print hists
    print $ length tailvisited
    -- print $ moveRope initrope 'R'
