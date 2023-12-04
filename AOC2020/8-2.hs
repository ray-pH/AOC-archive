import System.IO

data Program = Program { code :: [String]
                       , pointer :: Int
                       , pastpoint :: [Int]
                       , acc :: Int
                       } deriving Show

subset :: Eq a => Int -> Int -> [a] -> [a]
subset start end arr = take (end - start) (drop start arr)

changeset :: Eq a => Int -> a -> [a] -> [a]
changeset n e arr = let front = subset 0 n arr
                        back = subset (n+1) (length arr) arr
                     in front ++ [e] ++ back

readVal :: String -> Int
readVal ('+':xs) = read xs
readVal s = read s

repl :: Int -> [String] -> [String]
repl n arr
    | opcode == "nop" =  changeset n ("jmp " ++ val) arr
    | opcode == "jmp" =  changeset n ("nop " ++ val) arr
    | otherwise = arr
    where [opcode,val] = words (arr !! n)

isTerminating :: Program -> Bool
isTerminating pr = elem l (pastpoint res)
    where l = (length $ code pr) - 1
          res = runProgram pr

runCode :: Program -> String -> Int -> Program
runCode pr "nop" _   = Program (code pr) (p+1) (p:(pastpoint pr)) (acc pr)
           where p = pointer pr
runCode pr "acc" val = Program (code pr) (p+1) (p:(pastpoint pr)) (acc pr + val)
           where p = pointer pr
runCode pr "jmp" val = Program (code pr) (p+val) (p:(pastpoint pr)) (acc pr)
           where p = pointer pr

runProgram :: Program -> Program
runProgram pr
    | (elem (pointer pr) (pastpoint pr)) || ((pointer pr) >= length (code pr)) = pr
    | otherwise = let [opcode, rawval] = words (code pr !! pointer pr)
                      value = readVal rawval 
                   in runProgram $ runCode pr opcode value

main = do
    file <- openFile "8-input.txt" ReadMode
    -- file <- openFile "8-ex.txt" ReadMode
    cont <- hGetContents file
    let l = lines cont
    let n = [0..(length l)- 1]
    let replaced = [repl x l | x<-n]
    let programs = [Program cd 0 [] 0 | cd <- replaced]
    let solution = head [ p | p<-programs, isTerminating p ]
    print $ runProgram solution
    -- let pr = Program l 0 [] 0
    -- mapM_ print $ code pr
    -- let term = isTerminating pr
    -- print term
