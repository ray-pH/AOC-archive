-- flatten -> concat
-- iterN   -> iterate
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
     | elem x xs = unique xs
     | otherwise = x : unique xs

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

replaceAt :: Int -> a -> [a] -> [a]
replaceAt id val arr = l ++ [val] ++ r
    where (l, (x:r)) = splitAt id arr

indexesOf :: Eq a => a -> [a] -> [Int]
indexesOf v = map fst . filter ((== v) . snd) . zip [0..]


---- Arr2D -----
--
type Arr2D a = [[a]]

--        width(x) height(y) initVal
a2Dgen :: Int ->   Int ->    a ->    Arr2D a
a2Dgen  w h v = map (const row) [1..h]
    where row = map (const v) [1..w]
a2Dload :: Pos -> Arr2D a -> a
a2Dload (x,y) arr = (arr !! y) !! x
a2Dstore :: a -> Pos -> Arr2D a -> Arr2D a
a2Dstore val (x,y) arr = replaceAt y row'  arr
    where row  = arr !! y
          row' = replaceAt x val row
a2Dstorerow :: [a] -> Int -> Arr2D a -> Arr2D a
a2Dstorerow newrow y arr = replaceAt y newrow arr
