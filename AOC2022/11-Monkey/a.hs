import System.IO
import Data.List

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

iter :: Int -> (a -> a) -> a -> a
iter 0 _ val = val
iter n f val = iter (n-1) f $ f val

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

type FOperation = Int -> Int
type FTest      = Int -> Bool
--                   Items Operation  Test  (True,False) toThrow(target,item)  counter
data Monkey = Monkey [Int] FOperation FTest (Int, Int)   [(Int,Int)]           Int

getMonkeyItems    (Monkey items _ _ _ _ _)  = items
getMonkeyToThrow  (Monkey _ _ _ _ toThe _)  = toThe
getMonkeyCounter (Monkey _ _ _ _ _ counter) = counter
countMonkeyItem = length . getMonkeyItems

combineword :: [String] -> String
combineword [] = []
combineword (x:xs) = x ++ " " ++ combineword xs

genFOperation :: String -> FOperation
genFOperation str
  | l == "old" && r == "old" = (\x -> operator x x)
  | l == "old"               = operator (read r :: Int)
  | r == "old"               = operator (read l :: Int)
  where [l,operatorstr,r] = words str
        operator = strToOp operatorstr
        strToOp "*" = (*)
        strToOp "+" = (+)

genFTest :: String -> FTest
genFTest str = (==) 0 . (\x -> mod x num)
    where num = read $ last $ words str :: Int

-- Monkey 0:
--   Starting items: 79, 98
--   Operation: new = old * 19
--   Test: divisible by 23
--     If true: throw to monkey 2
--     If false: throw to monkey 3
genMonkey :: [String] -> Monkey
genMonkey arr = Monkey items operation test (iftrue, iffalse) [] 0
    where itemsstr  = combineword $ drop 2 $ words $ arr !! 1
          items     = read ("[" ++ itemsstr ++ "]") :: [Int]
          operation = genFOperation $ combineword $ drop 3 $ words $ arr !! 2
          test      = genFTest $ arr !! 3
          iftrue    = read $ last $ words $ arr !! 4
          iffalse   = read $ last $ words $ arr !! 5

monkeyInspect :: Monkey -> Monkey
monkeyInspect (Monkey (item:items') opf testf (iftrue,iffalse) toThrow counter)
    =          Monkey items' opf testf (iftrue, iffalse) toThrow' (counter+1)
    where afterop    = opf item
          afterbored = div afterop 3
          test       = testf afterbored :: Bool
          target     = if test then iftrue else iffalse
          toThrow'   = toThrow ++ [(target, afterbored)]

monkeyTurn :: Monkey -> Monkey
monkeyTurn monkey
    | (==) 0 $ countMonkeyItem monkey = monkey
    | otherwise = monkeyTurn $ monkeyInspect monkey

addItemToMonkey :: Int -> Monkey -> Monkey
addItemToMonkey val (Monkey items opf testf (iftrue,iffalse) toThrow counter)
    = (Monkey (items ++ [val]) opf testf (iftrue,iffalse) toThrow counter)
addItemToMonkeys :: (Int, Int) -> [Monkey] -> [Monkey]
addItemToMonkeys (target, val) monkeys = l ++ [monkey'] ++ r
    where l = take target monkeys
          r = drop (target+1) monkeys
          monkey' = addItemToMonkey val $ monkeys !! target

cleanThrowMonkey :: Monkey -> Monkey
cleanThrowMonkey (Monkey items opf testf (iftrue,iffalse) toThrow counter)
                = Monkey items opf testf (iftrue,iffalse) [] counter
getAllThrownClean :: [Monkey] -> ([Monkey], [(Int,Int)])
getAllThrownClean monkeys = (map cleanThrowMonkey monkeys, allThrows)
    where allThrows = flatten $ map getMonkeyToThrow monkeys

applyThrows :: [(Int,Int)] -> [Monkey] -> [Monkey]
applyThrows [] monkeys = monkeys
applyThrows (x:xs) monkeys = applyThrows xs $ addItemToMonkeys x monkeys

monkeysThrow :: [Monkey] -> [Monkey]
monkeysThrow monkeys = applyThrows toThrows cleanmonkeys
    where (cleanmonkeys, toThrows) = getAllThrownClean monkeys

doRound :: [Int] -> [Monkey] -> [Monkey]
doRound [] monkeys = monkeys
doRound (n:ns) monkeys = doRound ns monkeys''
    where l = take n monkeys
          r = drop (n+1) monkeys
          monkeyn   = monkeys !! n
          -- do inspection
          monkeyn'  = monkeyTurn monkeyn
          monkeys'  = l ++ [monkeyn'] ++ r
          -- do throws
          monkeys'' = monkeysThrow monkeys'

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        grouped  = splitBy [] lin
        monkeys  = map genMonkey grouped
        monkcount= length monkeys
        counter  = [0..(monkcount-1)]
        -- monk1    = monkeys !! 0
        -- monk1'   = monkeyTurn monk1
        -- monkeys' = monk1' : tail monkeys
        -- monkeys''= monkeysThrow monkeys'
        afterround20 = iter 20 (\m -> doRound counter m) monkeys
        counters     = map getMonkeyCounter afterround20
        sortedcounters = reverse $ sort counters
    mapM_ print $ map getMonkeyItems monkeys
    print "-----"
    mapM_ print $ map getMonkeyItems afterround20
    print "-----"
    mapM_ print $ map getMonkeyCounter afterround20
    print "-----"
    print sortedcounters
    print $ foldr (*) 1 $ take 2 sortedcounters
