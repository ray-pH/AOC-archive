import System.IO

data Inst = Inst { op::Char, val::Int } deriving Show
data Program = Program { loc::(Int,Int), heading::Char
                       , pointer::Int, insts::[Inst] }
                        deriving Show

toInst :: String -> Inst
toInst (op:v) = Inst op (read v :: Int)

manhattan :: (Int, Int) -> Int
manhattan (x,y) = (+) (abs x) (abs y)

getLeft :: Char -> Char
getLeft 'N' = 'W'
getLeft 'W' = 'S'
getLeft 'S' = 'E'
getLeft 'E' = 'N'

getRight :: Char -> Char
getRight 'N' = 'E'
getRight 'E' = 'S'
getRight 'S' = 'W'
getRight 'W' = 'N'

getTuple :: Char -> (Int,Int)
getTuple 'N' = (0,1) 
getTuple 'E' = (1,0)
getTuple 'S' = (0,-1)
getTuple 'W' = (-1,0)

mulTuple :: Int -> (Int,Int) -> (Int,Int)
mulTuple n (x,y) = (n*x, n*y)

sumTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumTuple (x,y) (a,b) = (x+a, y+b)

rotate :: Char -> Int -> Char -> Char
rotate 'R' 90 head = getRight head
rotate 'L' 90 head = getLeft head
rotate 'R' 270 head = rotate 'L' 90 head
rotate 'L' 270 head = rotate 'R' 90 head
rotate 'R' 180 head = rotate 'R' 90 $ rotate 'R' 90 head
rotate 'L' 180 head = rotate 'R' 180 head

runOp :: Char -> Int -> Program -> Program
runOp op val pr 
    | elem op "NEWS" =  Program loc' (heading pr) (pointer pr + 1) (insts pr)
    | op == 'F' = runOp (heading pr) val pr
    | otherwise = Program (loc pr) (rotate op val $ heading pr) (pointer pr + 1) (insts pr)
    where loc' = sumTuple (mulTuple val $ getTuple op) (loc pr)

runProgram :: Program -> Program
runProgram pr 
    | pointer pr >= (length $ insts pr) = pr
    | otherwise = runProgram $ runOp op val pr
    where Inst op val = insts pr !! pointer pr

main :: IO()
main = do
    file <- openFile "12-input.txt" ReadMode
    -- file <- openFile "12-ex.txt" ReadMode
    cont <- hGetContents file
    let insts = map toInst $ lines cont
        pr = Program (0,0) 'E' 0 insts
        las = runProgram pr
        p = loc las
    print las
    print $ manhattan p 
