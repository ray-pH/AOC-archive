import System.IO

data Ops = Noop | Noopi | Addx Int | Addi Int deriving Show
type State = Int
type Canvas = [Char]

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

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

draw :: Canvas -> [State] -> Canvas
draw cv [] = cv
draw cv (x:xs) = draw cv' xs
    where cvpointer = mod (length cv) 40
          pointeron = (cvpointer >= x-1) && (cvpointer <= x+1)
          cv'       = cv ++ [if pointeron then '#' else '.']

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpex1.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        parsed = parseCommand lin
        translated = translateCommand parsed
        result = 1 : runProgram 1 translated :: [State]
        canvas = draw "" result
        screen = splitEvery 40 canvas
    -- print lin
    print result
    mapM_ print screen
