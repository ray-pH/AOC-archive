import System.IO

data Ops = Noop | Noopi | Addx Int | Addi Int deriving Show
type State = Int

parseCommand :: [String] -> [Ops]
parseCommand [] = []
parseCommand (x:xs)
  | x   == "noop" = Noop : parseCommand xs
  | com == "addx" = (Addx (read valstr)) : parseCommand xs
  where [com,valstr] = words x

translateCommand :: [Ops] -> [Ops]
translateCommand [] = []
translateCommand (Noop : xs) = Noop : translateCommand xs
translateCommand ((Addx val):xs) = Noopi : (Addi val) : translateCommand xs

runProgram :: State -> [Ops] -> [State]
runProgram state [] = []
runProgram state (Noop:ops) = state : runProgram state ops
runProgram state (Noopi:ops) = state : runProgram state ops
runProgram state (Addi val:ops) = state' : runProgram state' ops
    where state' = state + val

evaluateImpCyc :: [Int] -> [Int] -> [Int]
evaluateImpCyc impcyc result = values
    where values = map (\c -> c * (result !! c)) impcyc

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpex1.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        parsed = parseCommand lin
        translated = translateCommand parsed
        result = 0 : 1 : runProgram 1 translated
        importantCycle = [20,60..(length result)]
        importantSignal = evaluateImpCyc importantCycle result
    -- print lin
    -- print translated
    print result
    print importantSignal
    print $ sum importantSignal
