import System.IO
import Data.Char

type Node = String
type Link = (Node,Node)

isCapital :: Char -> Bool
isCapital c = 'A' <= c && c <= 'Z'

isStrCapital :: String -> Bool
isStrCapital = and . map isCapital

parseLine :: String -> Link
parseLine = toTuple . words . map dToSpace
    where toTuple (x:y:[]) = (x,y)
          dToSpace :: Char -> Char
          dToSpace '-' = ' '
          dToSpace x   = x

isInLink :: Node -> Link -> Bool
isInLink x (a,b) = a == x || b == x

getLinks :: Node -> [Link] -> [Link]
getLinks n = filter (isInLink n)

getOtherSide :: Node -> [Link] -> [Node]
getOtherSide node = map (getOther node)
    where getOther :: Node -> (Node,Node) -> Node
          getOther n (a,b)
              | n == a = b
              | n == b = a

deploy :: [Link] -> [Node] -> Node -> Int
deploy cave history pos 
    | not (isStrCapital pos) && elem pos history = 0 -- visited small cave twice
    | pos == "end" = 1
    | otherwise = sum $ map (deploy cave (pos:history)) others
    where links  = getLinks pos cave :: [Link]
          others = getOtherSide pos links


main :: IO()
main = do
    -- file <- openFile "inpexc.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        links = map parseLine lin
    print $ deploy links [] "start"
