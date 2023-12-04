import System.IO


data Program = Program { code :: [String]
                       , pointer :: Int
                       , pastpoint :: [Int]
                       , acc :: Int
                       } deriving Show

readVal :: String -> Int
readVal ('+':xs) = read xs
readVal s = read s

runCode :: Program -> String -> Int -> Program
runCode pr "nop" _   = Program (code pr) (p+1) (p:(pastpoint pr)) (acc pr)
           where p = pointer pr
runCode pr "acc" val = Program (code pr) (p+1) (p:(pastpoint pr)) (acc pr + val)
           where p = pointer pr
runCode pr "jmp" val = Program (code pr) (p+val) (p:(pastpoint pr)) (acc pr)
           where p = pointer pr

runProgram :: Program -> Program
runProgram pr
    | elem (pointer pr) (pastpoint pr) = pr
    | otherwise = let [opcode, rawval] = words (code pr !! pointer pr)
                      value = readVal rawval 
                   in runProgram $ runCode pr opcode value
main = do
    file <- openFile "8-input.txt" ReadMode
    -- file <- openFile "8-ex.txt" ReadMode
    cont <- hGetContents file
    let l = lines cont
    let pr = (Program l 0 [] 0)
    let res = runProgram pr
    print res
