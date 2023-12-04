import System.IO
import Data.List

data End = Left | Right
    deriving (Show)
type Rule = (String,Char)
type Pair = (String,Int)
type Poly = [Pair]
-- CH -> B
--
parseInsertion :: String -> Rule
parseInsertion st = (pair,c)
    where (pair:_:(c:""):[]) = words st

fconcat :: Eq a => [(a,Int)] -> [(a,Int)]
fconcat [] = []
fconcat (p:[]) = [p]
fconcat (p:q:ps)
    | sp == sq = fconcat ((sp,ip+iq):ps)
    | otherwise = p : fconcat (q:ps)
    where (sp,ip) = p
          (sq,iq) = q

concatPoly :: Poly -> Poly
concatPoly = fconcat . sort

initPoly :: String -> Poly
initPoly [] = []
initPoly (x:[]) = []
initPoly (a:b:rs) = concatPoly (newp: initPoly (b:rs))
    where newp = (a:b:"", 1)

applyRule :: Pair -> Rule -> [Pair]
applyRule (str,x) (_,c) = [(s1,x),(s2,x)]
    where (a:b:"") = str
          s1 = a:c:""
          s2 = c:b:""

fstep :: [Rule] -> Poly -> Poly
fstep _ [] = []
fstep rules (p:ps) = next ++ fstep rules ps
    where (str,x) = p
          fitrule = filter (\(s,c) -> s == str) rules :: [Rule]
          next = if 0 == length fitrule then [p] else applyRule p (head fitrule)

step :: [Rule] -> Poly -> Poly
step rules = concatPoly . fstep rules

stepN :: Int -> [Rule] -> Poly -> Poly
stepN 0 _ poly = poly
stepN x rules poly = stepN (x-1) rules (step rules poly)

concatCount :: [(Char,Int)] -> [(Char,Int)]
concatCount = fconcat . sort

unwrapCount :: Poly -> [(Char,Int)]
unwrapCount [] = []
unwrapCount (p:ps) = [(a,x), (b,x)] ++ unwrapCount ps
    where (str,x) = p
          (a:b:"") = str

fcountElement :: Poly -> [(Char,Int)]
fcountElement = concatCount . unwrapCount

countElement :: Poly -> Char -> Char -> [(Char,Int)]
countElement poly left right = map (\(c,x) -> (c,div x 2)) $ map (f left right) $ fcountElement poly
    where f :: Char -> Char -> (Char,Int) -> (Char,Int)
          f l r (c,x)
            | c == l || c == r = (c,x+1)
            | otherwise = (c,x)

getValue :: Poly -> Char -> Char -> Int
getValue poly left right = maxx - minn
    where counts  = countElement poly left right
          flipped = map (\(a,b) -> (b,a)) counts
          sorted = sort flipped
          (minn,_) = head sorted
          (maxx,_) = last sorted

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let (pol:_:lin) = lines cont
        poly  = initPoly pol
        leftc = head pol
        rightc = last pol
        rules = map parseInsertion lin
        aften = stepN 40 rules poly
        counts = countElement aften leftc rightc
        val = getValue aften leftc rightc
    print counts
    print val
