import System.IO

type Range = (Integer,Integer)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

fullyContain :: Range -> Range -> Bool
fullyContain (a,b) (m,n) = abinmn || mninab
    where abinmn = m <= a && b <= n
          mninab = a <= m && n <= b

overlap :: Range -> Range -> Bool
overlap (a,b) (m,n) = not (b < m || n < a)

parseRange :: String -> Range
parseRange str = (read a, read b)
    where [a,b] = splitBy '-' str

parseLine :: String -> (Range,Range)
parseLine line = (parseRange str1, parseRange str2)
    where [str1,str2] = splitBy ',' line

lineContain :: String -> Bool
lineContain line = overlap r1 r2
    where (r1,r2) = parseLine line

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        con = map lineContain lin
        count = length $ filter (\x -> x) con
    print lin
    print count
