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
        maybe     = genUnique $ explode commons
        safe      = [ x | x <- ingrs, not $ elem x maybe ]
        allsafe   = [ x | x <- rawingrs, elem x safe ]
        -- possib    = 
    -- mapM_ print foods
    -- print ingrs
    print allgs
    -- mapM_ print fwithalgs
    mapM_ print commons
    print maybe
    print safe
    print allsafe
    print $ length allsafe
