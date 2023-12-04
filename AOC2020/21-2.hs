module Main where

import System.IO
import Data.List

genUnique :: Eq a => [a] -> [a]
genUnique [] = []
genUnique (x:xs)
    | elem x xs = genUnique xs
    | otherwise = x : genUnique xs

explode :: [[a]] -> [a]
explode = foldr (++) []

data Food = Food { ingridients :: [String], allergent :: [String] }
        deriving Show

type Ingridient = String
type Allergent = String

getAllergent :: String -> [Allergent]
getAllergent st = let w = words st
                      nocontain = tail w
                   in map f nocontain
                  where f :: String -> String
                        f st = if last st == ','
                                  then init st
                                  else st 

toFood :: String -> Food
toFood st = case elemIndex '(' st of
              Nothing -> Food [] []
              Just val -> let front = init $ take val st
                              back  = tail $ init $ drop val st
                           in Food (words front) (getAllergent back) 

getFoodThatContain :: [Food] -> Allergent -> [Food]
getFoodThatContain foods all = filter (\y -> elem all $ allergent y) foods

getCommonIngridient :: [Food] -> [Ingridient]
getCommonIngridient [] = []
getCommonIngridient foods = let ingrs   = map ingridients foods
                                uni     = head ingrs
                                f :: [Ingridient] -> [Ingridient] -> [Ingridient]
                                f i1 i2 = [ x | x<-i1, y<-i2, x==y ]
                             in foldr f uni $ tail ingrs 

type Pair = (Allergent,[Ingridient])

getFinal :: [Pair] -> [Pair]
getFinal list = [ (al,i) | (al,i)<-list, length i == 1 ]

stripFinal :: [Pair] -> [Pair]
stripFinal list = [ (al,i) | (al,i)<-list, length i /= 1 ]

sterilize :: [Pair] -> Pair -> Pair
sterilize final (all,ingrs) = let finalingrs = [ head is | (al,is) <- final ]
                                  filtered = [ i | i <- ingrs, not $ elem i finalingrs ]
                               in (all,filtered)
                            

runCheck :: [Pair] -> [Pair] -> [Pair]
runCheck final [] = final
runCheck final left = let newfinal = final ++ getFinal left
                          newleft  = stripFinal left
                          left'    = map (sterilize newfinal) newleft
                       in runCheck newfinal left' 

addWithDelim :: a -> [a] -> [a] -> [a]
addWithDelim e l1 l2 = l1 ++ e : l2

main :: IO()
main = do
    file <- openFile "21-input.txt" ReadMode
    -- file <- openFile "21-ex.txt" ReadMode
    cont <- hGetContents file
    let l         = lines cont
        foods     = map toFood l
        f         = \y ->  explode $ map y foods
        rawingrs  = f ingridients
        ingrs     = genUnique rawingrs
        allgs     = genUnique $ f allergent
        fwithalgs = map (\y -> getFoodThatContain foods y) allgs
        commons   = map getCommonIngridient fwithalgs
        zipped    = zip allgs commons
        result    = runCheck [] zipped
        --[("dairy",["mxmxvkd"]),("fish",["sqjhc"]),("soy",["fvjkl"])]
        sortf     = (\(a,_) (b,_) -> compare a b)
        sorted    = sortBy sortf result
        output    = init $ foldr (addWithDelim ',') "" $ map (\(_,x:_) -> x) sorted
    mapM_ print zipped
    -- print $ getFinal zipped
    print sorted
    print output
