import System.IO

type Tank = [(Int,Int)] --- [(Id<time>,Count)]
emptyTank = zip [0..8] (repeat 0)

parseInput :: String -> [Int]
parseInput lin = map read $ words $ map commaToSpace $ lin
    where commaToSpace :: Char -> Char
          commaToSpace ',' = ' '
          commaToSpace x   = x

genTank :: [Int] -> Tank -> Tank
genTank [] tank = tank
genTank (x:xs) tank = genTank xs $ map (\tuple -> inc x tuple) tank
    where inc :: Int -> (Int,Int) -> (Int,Int)
          inc n (id,count)
              | n == id   = (id, count+1)
              | otherwise = (id, count  )

iterAge :: Tank -> Tank
iterAge (_:[]) = [(8,0)]
iterAge (f:fs) = (id,count) : iterAge fs
    where (id, _ )   = f
          (_ , count) = head fs

iterBreed :: Int -> Tank -> Tank
iterBreed _ [] = []
iterBreed breed (age:ages)
    | id == 6 || id == 8 = (id,count+breed): next
    | otherwise = (id,count): next
    where (id,count) = age
          next = iterBreed breed ages

iterDay :: Tank -> Tank
iterDay tank = iterBreed breed $ iterAge tank
    where (_,breed) = tank!!0

simulate :: Int -> Tank -> Tank
simulate 0 tank = tank
simulate x tank = simulate (x-1) $ iterDay tank

countFish :: Tank -> Int
countFish [] = 0
countFish ((_,count):fs) = count + countFish fs

main :: IO()
main = do
    file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin  = lines cont
        init = parseInput $ lin!!0
        initTank = genTank init emptyTank
        endt = simulate 256 initTank
    print initTank
    print $ countFish endt
