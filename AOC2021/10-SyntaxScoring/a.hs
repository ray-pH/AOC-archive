import System.IO

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

filterDone :: String -> String
filterDone = filter (\x -> elem x ">]})")

value :: String -> Int
value ""      = 0
value (')':_) = 3
value (']':_) = 57
value ('}':_) = 1197
value ('>':_) = 25137
value _       = 0

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        simpl = map simplify lin
        filt  = map filterDone simpl
        val   = map value filt
    print $ sum val
