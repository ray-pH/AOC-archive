import System.IO
import Data.List

simplifyPass :: String -> String
simplifyPass "" = ""
simplifyPass ('(':')':rs) = simplifyPass rs
simplifyPass ('[':']':rs) = simplifyPass rs
simplifyPass ('<':'>':rs) = simplifyPass rs
simplifyPass ('{':'}':rs) = simplifyPass rs
simplifyPass (r:rs) = r : simplifyPass rs

simplify :: String -> String
simplify st
    | newst == st = newst
    | otherwise   = simplify newst
    where newst   = simplifyPass st

isNormal :: String -> Bool
isNormal st = not $ elem ')' st || elem ']' st || elem '}' st || elem '>' st

charValue :: Char -> Int
charValue '(' = 1
charValue '[' = 2
charValue '{' = 3
charValue '<' = 4

getValue :: String -> Int
getValue "" = 0
getValue (c:cs) = (charValue c) + (5 * getValue cs)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        simpl = map simplify lin
        filt  = filter isNormal simpl
        val   = map getValue filt
        len   = length val
    -- print filt
    print $ (sort val) !! div len 2
