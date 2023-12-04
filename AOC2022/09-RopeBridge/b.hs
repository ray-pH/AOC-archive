import System.IO

type Pos  = (Int,Int)
type Link = (Pos,Pos)
type Rope = [Pos]

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

updateLink :: Pos -> Pos -> Pos
updateLink (hx, hy) (tx, ty)
  | hx == tx && hy - ty > 1  = (tx, ty+1)
  | hx == tx && hy - ty < -1 = (tx, ty-1)
  | hy == ty && hx - tx > 1  = (tx+1, ty)
  | hy == ty && hx - tx < -1 = (tx-1, ty)
  | isConnected = (tx,ty)
  | otherwise  = diagonalxTail $ diagonalyTail (tx,ty)
  where isConnected = and [abs (hx-tx) <= 1, abs (hy-ty) <= 1]
        diagonalxTail :: Pos -> Pos
        diagonalxTail (txx,tyy)
          | hx - txx > 0 = (txx+1, tyy)
          | hx - txx < 0 = (txx-1, tyy)
        diagonalyTail :: Pos -> Pos
        diagonalyTail (txx,tyy)
          | hy - tyy > 0 = (txx, tyy+1)
          | hy - tyy < 0 = (txx, tyy-1)

updateRope :: Rope -> Rope
updateRope (x:[])   = [x]
updateRope (a:b:xs) = a : updateRope (b':xs)
    where b' = updateLink a b

moveHead :: Rope -> Char -> Rope
moveHead ((hx,hy) : xs) 'U' = (hx,hy+1) : xs
moveHead ((hx,hy) : xs) 'D' = (hx,hy-1) : xs
moveHead ((hx,hy) : xs) 'R' = (hx+1,hy) : xs
moveHead ((hx,hy) : xs) 'L' = (hx-1,hy) : xs

moveRope :: Rope -> Char -> Rope
moveRope rope c = updateRope $ moveHead rope c

moveRopeWithHistory :: [Rope] -> [Char] -> [Rope]
moveRopeWithHistory history [] = history
moveRopeWithHistory history (c:cs) = moveRopeWithHistory (history ++ [newrope]) cs
    where currentrope = last history
          newrope     = moveRope currentrope c

main :: IO()
main = do
    -- file <- openFile "inpex1.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        movements = parseMovement lin
        movements' = expandMovement movements
        initrope  = map (\x -> (0,0)) [1..10]
        hists     = moveRopeWithHistory [initrope] movements'
        tailhist  = map last hists
        tailvisited = unique tailhist
    print movements'
    mapM_ print hists
    print $ length tailvisited
    -- print $ moveLink initrope 'R'
