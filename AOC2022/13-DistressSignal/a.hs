import System.IO
import Data.Char

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

data Signal = I Int | List [Signal] deriving Show

data Booj   = Truj | Falsj | Nexj   deriving Show

isNexj :: Booj -> Bool
isNexj Nexj = True
isNexj Truj = False
isNexj Falsj= False

boojToBool :: Booj -> Bool
boojToBool Truj  = True
boojToBool Falsj = False

listIntTranslate :: [Int] -> Signal
listIntTranslate xs = List (map (\x -> I x) xs)

fparseBracket :: String -> Int -> String -> [String]
fparseBracket accstr 0 "" = [accstr]
fparseBracket accstr n (c:cs)
  | c == ',' && n == 0 = accstr : fparseBracket "" n cs
  | c == '['  = fparseBracket (accstr ++ "[") (n+1) cs
  | c == ']'  = fparseBracket (accstr ++ "]") (n-1) cs
  | otherwise = fparseBracket (accstr ++ [c]) n     cs

parseBracket :: String -> [String]
parseBracket = fparseBracket "" 0 . remOuter
remOuter = tail . init

parseSignal :: String -> Signal
parseSignal str
    | str == "[]"     = List []
    | all isDigit str = I (read str :: Int)
    | otherwise       = List (map parseSignal $ parseBracket str)

compareSignal :: Signal -> Signal -> Booj
compareSignal (I x) (I y)
    | x < y  = Truj
    | x > y  = Falsj
    | x == y = Nexj
compareSignal (List []) (List []) = Nexj
compareSignal (List []) (List li) = Truj
compareSignal (List li) (List []) = Falsj
compareSignal (List (x:xs)) (List (y:ys))
    | isNexj res = compareSignal (List xs) (List ys)
    | otherwise  = res
    where res = compareSignal x y
compareSignal (List li) (I i) = compareSignal (List li)      (List [(I i)])
compareSignal (I i) (List li) = compareSignal (List [(I i)]) (List li) 

compareSignalGroup :: [Signal] -> Bool
compareSignalGroup (x:y:_) = boojToBool $ compareSignal x y
-- fcompareSignalGroup :: [Signal] -> Bool
fcompareSignalGroup (x:y:_) = compareSignal x y

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grouped = splitBy "" lin
        parsed  = map (map parseSignal) grouped
        compared = map compareSignalGroup parsed
        correctId = map fst $ filter snd $ zip [1..] compared
    mapM_ print parsed
    print correctId
    print $ sum correctId
