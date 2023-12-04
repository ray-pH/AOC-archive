import System.IO
import Data.List

type Card = [[Int]]
type BoolCard = [[Bool]]
emptyCard = []

boolToNotInt :: Bool -> Int
boolToNotInt True  = 0
boolToNotInt False = 1

parseCard :: [String] -> Card -> [Card]
parseCard []      card = [card]
parseCard ("":xs) card = card: parseCard xs emptyCard
parseCard (x:xs)  card = parseCard xs (card ++ [readLine x])
    where readLine = map (read :: String -> Int) . words

checkWin :: BoolCard -> Bool
checkWin boolCard = horizontal || vertical
    where horizontal = or  $ map and boolCard
          vertical   = or  $ map and $ transpose boolCard
          -- diag1      = and $ map (\(id,arr) -> arr!!id) $ zip [0..] boolCard
          -- diag2      = and $ map (\(id,arr) -> arr!!(length boolCard - id - 1)) $ zip [0..] boolCard

cardMask :: [Int] -> Card -> BoolCard -> BoolCard
cardMask [] _ boolCard = boolCard
cardMask (x:xs) card boolCard = cardMask xs card newBoolCard
    where boolMask    = map ( map(\a -> a == x) ) card
          zippedCard  = map (\(a,b) -> zip a b) $ zip boolMask boolCard
          newBoolCard = map (map (\(a,b) -> a||b) ) zippedCard
          
bingo :: [Int] -> [Int] -> [Card] -> [Bool] -> Int
bingo draws stack cards [] = bingo draws stack cards (take (length cards) $ repeat False)
bingo (d:draws) stack cards prevWins
    | and wins   = summ * last stack
    | otherwise = bingo draws (stack ++ [d]) cards wins
    where wins  = map (\c -> checkWin $ cardMask stack c emptyBoolCard) cards
          emptyBoolCard = map (map (\_ -> False)) (cards!!0)
          andWin    = map (\(a,b) -> a && b) $ zip wins prevWins
          (winId,_) = (filter (\(_,b) -> b) $ zip [0..] $ map not andWin)!!0
          winningCard = cards!!winId
          winningMasked = cardMask stack winningCard emptyBoolCard
          maskedInteger = map (map boolToNotInt) winningMasked
          winningZipped = map (\(a,b) -> zip a b) $ zip winningCard maskedInteger
          summ          = sum $ map (\(a,b) -> a*b)$ concat winningZipped

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        drawStr:_:xs = lin
        draws = map (read :: String -> Int) $ words $ map (commaToSpace) drawStr
            where commaToSpace :: Char -> Char
                  commaToSpace ',' = ' '
                  commaToSpace c = c
        cards = parseCard xs emptyCard
    print $ bingo draws [] cards []
