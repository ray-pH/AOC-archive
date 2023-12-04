import System.IO
import Data.List (sort)
import Data.Maybe (isNothing)
import Debug.Trace (trace)

data Dir = DL | DR deriving (Show, Eq, Ord)
data Snail = SVal  Int
           | SPair Snail Snail
           deriving (Eq)

instance Show Snail where
    show (SVal val) = show val
    show (SPair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

findLeftPartner :: String -> Int -> Int -> Int
findLeftPartner _ 0 id = id
findLeftPartner (c:cs) counter id
    | c == '[' = findLeftPartner cs (counter+1) (id+1)
    | c == ']' = findLeftPartner cs (counter-1) (id+1)
    | otherwise = findLeftPartner cs counter (id+1)

parseVal :: String -> Snail
parseVal s = SVal (read s)

parsePair :: String -> Snail
parsePair str = SPair lSnail rSnail
    where stripped = (init . tail) str
          leftIsPair  = '[' == head stripped
          --
          pairID   = findLeftPartner (tail stripped) 1 1
          nopairID = length $ takeWhile (/= ',') stripped
          rID = if leftIsPair then pairID else nopairID
          leftStr = take rID stripped
          rightStr = drop (rID+1) stripped
          lSnail = parseString leftStr
          rSnail = parseString rightStr

parseString :: String -> Snail
parseString s
    | '[' == head s = parsePair s
    | otherwise     = parseVal s

genDirs :: Snail -> [Dir] -> [[Dir]]
genDirs (SVal _) dirs = [dirs]
genDirs (SPair ls rs) dirs = ldirs ++ rdirs
    where ldirs = genDirs ls (dirs++[DL])
          rdirs = genDirs rs (dirs++[DR])

searchSplit :: Snail -> [Dir] -> [[Dir]]
searchSplit (SVal v) dirs
    | v > 9     = [dirs]
    | otherwise = []
searchSplit (SPair ls rs) dirs = ldirs ++ rdirs
    where ldirs = searchSplit ls (dirs++[DL])
          rdirs = searchSplit rs (dirs++[DR])

searchExplode :: Snail -> [Dir] -> Int -> [[Dir]]
searchExplode (SVal _) _ _ = []
searchExplode (SPair _ _) dirs 4 = [dirs]
searchExplode (SPair ls rs) dirs lv = ldirs ++ rdirs
    where ldirs = searchExplode ls (dirs++[DL]) (lv+1)
          rdirs = searchExplode rs (dirs++[DR]) (lv+1)

getLeft :: Snail -> [Dir] -> Maybe [Dir]
getLeft sn dir = f dir dirs
    where dirs = genDirs sn []
          f :: [Dir] -> [[Dir]] -> Maybe [Dir]
          f _ [] = Nothing
          f _ (d:[]) = Nothing
          f di (d:ds)
            | di++[DL] == (head ds) = Just d
            | otherwise             = f di ds
getRight :: Snail -> [Dir] -> Maybe [Dir]
getRight sn dir = f dir dirs
    where dirs = genDirs sn []
          f :: [Dir] -> [[Dir]] -> Maybe [Dir]
          f _ [] = Nothing
          f _ (d:[]) = Nothing
          f di (d:ds)
            | di++[DR] == d = Just (head ds)
            | otherwise     = f di ds

getSnail :: Snail -> [Dir] -> Snail
getSnail sn [] = sn
getSnail (SPair ls rs) (DR:ds) = getSnail rs ds
getSnail (SPair ls rs) (DL:ds) = getSnail ls ds

editSnail :: Snail -> [Dir] -> Snail -> Snail
editSnail (SPair ls rs) (DR:[]) snew = SPair ls snew
editSnail (SPair ls rs) (DL:[]) snew = SPair snew rs
editSnail (SPair ls rs) (DR:ds) snew = SPair ls (editSnail rs ds snew)
editSnail (SPair ls rs) (DL:ds) snew = SPair (editSnail ls ds snew) rs

-- splitSnail :: Snail -> Snail
-- splitSnail (SPair ls rs) = SPair (splitSnail ls) (splitSnail rs)
-- splitSnail (SVal val)
    -- | val > 0 = SPair (SVal lval) (SVal rval)
    -- | otherwise = (SVal val)
    -- where lval = div val 2
          -- rval = div (val+1) 2

splitSnail :: Snail -> Snail
splitSnail sroot
    | 0 == length toSplit = sroot
    | otherwise = editSnail sroot dir (SPair (SVal lval) (SVal rval))
    where toSplit = searchSplit sroot []
          dir = head $ sort toSplit
          (SVal val) = getSnail sroot dir
          lval = div val 2
          rval = div (val+1) 2


explodeSnail :: Snail -> Snail
explodeSnail sroot 
    | 0 == length toExplode = sroot
    | otherwise = newsroot
    where toExplode = searchExplode sroot [] 0
          dir = head $ sort toExplode
          -- doExplode
          SPair (SVal lexp) (SVal rexp) = getSnail sroot dir
          -- left
          ldir = getLeft sroot dir :: Maybe [Dir]
          (Just jldir) = ldir
          SVal plval = getSnail sroot jldir
          sroot' = if isNothing ldir then sroot else editSnail sroot jldir (SVal (plval + lexp))
          -- right
          rdir = getRight sroot dir :: Maybe [Dir]
          (Just jrdir) = rdir
          SVal prval = getSnail sroot jrdir
          sroot'' = if isNothing rdir then sroot' else editSnail sroot' jrdir (SVal (prval + rexp))
          -- explode
          newsroot = editSnail sroot'' dir (SVal 0)

stepSnail :: Snail -> Snail
stepSnail snail
    | 0 < length toExplode = explodeSnail snail
    | 0 < length toSplit   = splitSnail snail
    | otherwise = snail
    where toExplode = searchExplode snail [] 0
          toSplit = searchSplit snail []

reduce :: Snail -> Snail
reduce snail
    | snail == snail' = snail
    | otherwise = reduce snail'
    where snail' = stepSnail snail

addSnail :: Snail -> Snail -> Snail
addSnail s1 s2 = reduce (SPair (reduce s1) (reduce s2))

calcMagnitude :: Snail -> Int
calcMagnitude (SVal val) = val
calcMagnitude (SPair ls rs) = 3*(calcMagnitude ls) + 2*(calcMagnitude rs)

main :: IO()
main = do
    -- file <- openFile "inpexplode.txt" ReadMode
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpexb.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        snails = map parseString lin
        pairs = [(a,b) | a<-snails, b<-snails, a /= b]
        sums = map (\(a,b) -> addSnail a b) pairs
        mags = map calcMagnitude sums
        maxmags = maximum mags
    print mags
    print maxmags
