module Main where

snum :: Int
snum = 20201227

getLoopSize :: Int -> Int -> Int -> Int -> Int
getLoopSize target subj val loop = let val' = mod (val*subj) snum
                                    in if val' == target
                                          then loop+1
                                          else getLoopSize target subj val' (loop+1)


transform :: Int -> Int -> Int -> Int
transform key val loop
    | loop <= 0 = val
    | otherwise = transform key (mod (val*key) snum) (loop-1)

main = do
    let cardloop = getLoopSize 15113849 7 1 0
        doorloop = getLoopSize 4206373 7 1 0
    print cardloop
    print doorloop
    print $ transform 15113849 1 doorloop
