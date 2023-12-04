import System.IO
import Data.Char
import Debug.Trace

type ALU = (Int,Int,Int,Int,[Int])
data Arg = Num Int | Reg Char | Noarg deriving Show
type Op = (String,Char,Arg)

toInp :: Int -> [Int]
toInp = map (\x -> ord x - ord '0') . show

parseArg :: String -> Arg
parseArg s
   | elem s ["x","y","z","w"] = Reg (head s)
   | otherwise = Num (read s)

parseOp :: String -> Op
parseOp str
    | length w == 2 = (op,head reg, Noarg)
    | length w == 3 = (op,head reg, parseArg $ last w)
    where w = words str
          (op:reg:_) = w

putVal :: ALU -> Char -> Int -> ALU
putVal (x,y,z,w,buff) 'x' n = (n,y,z,w,buff)
putVal (x,y,z,w,buff) 'y' n = (x,n,z,w,buff)
putVal (x,y,z,w,buff) 'z' n = (x,y,n,w,buff)
putVal (x,y,z,w,buff) 'w' n = (x,y,z,n,buff)

getVal :: ALU -> Char -> Int
getVal (x,y,z,w,_) 'x' = x
getVal (x,y,z,w,_) 'y' = y
getVal (x,y,z,w,_) 'z' = z
getVal (x,y,z,w,_) 'w' = w

getArg :: ALU -> Arg -> Int
getArg _ (Num n) = n
getArg alu (Reg r) = getVal alu r

runOp :: ALU -> Op -> ALU
runOp (x,y,z,w,(b:bs)) ("inp",reg,Noarg) = putVal (x,y,z,w,bs) reg b
runOp alu ("add",reg,arg) = putVal alu reg (getVal alu reg + getArg alu arg)
runOp alu ("mul",reg,arg) = putVal alu reg (getVal alu reg * getArg alu arg)
runOp alu ("div",reg,arg) = putVal alu reg (getVal alu reg `div` getArg alu arg)
runOp alu ("mod",reg,arg) = putVal alu reg (getVal alu reg `mod` getArg alu arg)
runOp alu ("eql",reg,arg) = putVal alu reg (if (getVal alu reg == getArg alu arg) then 1 else 0)

runSystem :: ALU -> [Op] -> ALU
runSystem alu [] = alu
runSystem alu (op:ops) = runSystem alu' ops
    where alu' = runOp alu op

initAlu :: [Int] -> ALU
initAlu m = (0,0,0,0,m)

isValid :: ALU -> Bool
isValid (_,_,z,_,_) = z == 0

noZero :: [Int] -> Bool
noZero  = not . elem 0
main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        ops = map parseOp lin
        modelnumbers = filter noZero $ map toInp [99999999999999,99999999999998..]
        ran = map (\m -> runSystem (initAlu m) ops) modelnumbers
    mapM_ print $ zip (takeWhile (not . isValid) ran) modelnumbers
