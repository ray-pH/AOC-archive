module Main where

import System.IO

data Inst = Inst { op::Char, val::Int } deriving Show
data Ship = Ship { loc::(Int,Int), wayPoint::(Int,Int)
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

subTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
subTuple (x,y) (a,b) = (x-a, y-b)

rotate :: Char -> Int -> (Int,Int) -> (Int, Int) -> (Int, Int)
rotate c 90 (x0,y0) (wx,wy) = (x0+x'', y0+y'') 
    where (x',y') = (wx-x0, wy-y0)
          (x'',y'') = if c == 'R' then (y',-x') else (-y', x')
rotate _ 180 (x0,y0) (wx,wy) = (2*x0 - wx, 2*y0 - wy)
rotate 'R' 270 a b = rotate 'L' 90 a b
rotate 'L' 270 a b = rotate 'R' 90 a b

runOp :: Char -> Int -> Ship -> Ship
runOp op val pr 
    | elem op "NEWS" = Ship (loc pr) wp'' (pointer pr + 1) (insts pr)
    | op == 'F'      = Ship loc'     wp'  (pointer pr + 1) (insts pr)
    | otherwise = Ship (loc pr) (rotate op val (loc pr) (wayPoint pr)) (pointer pr + 1) (insts pr)
    where head = subTuple (wayPoint pr) (loc pr)
          loc' = sumTuple (mulTuple val $ head) (loc pr)
          wp'  = sumTuple (mulTuple val $ head) (wayPoint pr)
          wp'' = sumTuple (mulTuple val $ getTuple op) (wayPoint pr)

runProgram :: Ship -> Ship
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
        pr = Ship (0,0) (10,1) 0 insts
        las = runProgram pr
        p = loc las
    print las
    print $ manhattan p 
