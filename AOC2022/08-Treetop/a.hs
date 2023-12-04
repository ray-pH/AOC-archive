import System.IO
import Data.List

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

genSeries :: [Char] -> [[Char]]
genSeries str = map (\x -> take x str) [1..(length str)]

isVisible :: [Char] -> Bool
isVisible str  = and $ map (\x -> x < val) prev
    where prev = init str
          val  = last str

getVisibleLine :: [Char] -> [Bool]
getVisibleLine = map isVisible . genSeries

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        fromleft  = map getVisibleLine lin
        fromright = map reverse $ map getVisibleLine $ map reverse lin
        fromtop   = transpose $ map getVisibleLine $ transpose lin
        frombot   = transpose $ map reverse $ map getVisibleLine $ map reverse $ transpose lin
        flatfromleft  = flatten fromleft 
        flatfromright = flatten fromright
        flatfromtop   = flatten fromtop  
        flatfrombot   = flatten frombot  
        zipped    = zip4 flatfromleft flatfromright flatfromtop flatfrombot
        ored      = map (\(a,b,c,d) -> or [a,b,c,d]) zipped
        visibcount = length $ filter id ored
    mapM_ print lin
    print visibcount
