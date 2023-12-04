import System.IO
import Debug.Trace

data Packet = Pack Int Int (Maybe Int) (Maybe Int) [Packet] 
    deriving Show
-- version,id,lid,value,ops

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

c2i :: Char -> Int
c2i '0' = 0
c2i '1' = 1

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

readBinstr :: String -> Int
readBinstr = sum . map (\(a,b) -> b*2^a) . zip [0..] . reverse . map c2i

chexToBin :: Char -> String
chexToBin '0' = "0000"
chexToBin '1' = "0001"
chexToBin '2' = "0010"
chexToBin '3' = "0011"
chexToBin '4' = "0100"
chexToBin '5' = "0101"
chexToBin '6' = "0110"
chexToBin '7' = "0111"
chexToBin '8' = "1000"
chexToBin '9' = "1001"
chexToBin 'A' = "1010"
chexToBin 'B' = "1011"
chexToBin 'C' = "1100"
chexToBin 'D' = "1101"
chexToBin 'E' = "1110"
chexToBin 'F' = "1111"

hexToBin :: String -> String
hexToBin = flatten . map chexToBin

fparseLiteralVal :: String -> (String,String) -- (Value,Rest)
fparseLiteralVal rawbin
    | length rawbin < 5 = ("",rest)
    | flag == '0' = (dat,rest)
    | otherwise   = (dat ++ ndat, nrest)
    where (bits,rest) = splitAt 5 rawbin
          (flag:dat) = bits
          (ndat,nrest) = fparseLiteralVal rest
parseLiteralVal :: String -> (Int,String)
parseLiteralVal = (\(a,b) -> (readBinstr a, b)) . fparseLiteralVal

parseOperatorLen :: Int -> String -> ([Packet], String)
parseOperatorLen 0   rawbin = ([],rawbin)
parseOperatorLen len rawbin = (p:npacks, nrest)
    where (p,rest) = parsePacket rawbin
          dl       = length rawbin - length rest :: Int
          next     = parseOperatorLen (len-dl) rest :: ([Packet],String)
          (npacks, nrest) = next

parseOperatorNum :: Int -> String -> ([Packet], String)
parseOperatorNum 0   rawbin = ([],rawbin)
parseOperatorNum num rawbin = (p:npacks, nrest)
    where (p,rest) = parsePacket rawbin
          next     = parseOperatorNum (num-1) rest :: ([Packet],String)
          (npacks, nrest) = next

parseOperator :: Int -> Int -> String -> (Packet,String)
parseOperator packetVersion packetID rawbin
    | lID == 0 = (lenPack, lenrest')
    | lID == 1 = (numPack, numrest')
    where (lIDstr:rest) = rawbin
          lID = c2i lIDstr
          -- opLen
          (lenstr,lenrest) = splitAt 15 rest
          len = readBinstr lenstr
          (lenPacks,lenrest') = parseOperatorLen len lenrest
          lenPack = Pack packetVersion packetID (Just lID) Nothing lenPacks
          -- opNum
          (numstr,numrest) = splitAt 11 rest
          num = readBinstr numstr
          (numPacks, numrest') = parseOperatorNum num numrest
          numPack = Pack packetVersion packetID (Just lID) Nothing numPacks

parsePacket :: String -> (Packet,String) -- packet, rest
parsePacket rawbin
    | packetID == 4 = (litPack, litRest)
    | otherwise     = parseOperator packetVersion packetID rest'
    where (packetVersionStr,rest) = splitAt 3 rawbin
          packetVersion = readBinstr packetVersionStr
          (packetIDStr,rest') = splitAt 3 rest
          packetID = readBinstr packetIDStr
          -- literal
          (litVal,litRest) = parseLiteralVal rest'
          litPack = Pack packetVersion packetID Nothing (Just litVal) []
          -- operator

evalPacket :: Packet -> Int
evalPacket packet
    | id == 0 = sum contVals
    | id == 1 = product contVals
    | id == 2 = minimum contVals
    | id == 3 = maximum contVals
    | id == 4 = litVal
    | id == 5 = b2i (v1 > v2)
    | id == 6 = b2i (v1 < v2)
    | id == 7 = b2i (v1 == v2)
    where Pack _ id _ val contPackets = packet
          contVals = map evalPacket contPackets
          Just litVal = val
          -- for functions with two input
          (v1:v2:_) = contVals

main :: IO()
main = do
    -- file <- openFile "inpexb.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let lin = lines cont
        inp = lin !! 0
        bin = hexToBin inp
        (parsed,trash) = parsePacket bin
        result = evalPacket parsed
    print inp
    print parsed
    print trash
    print result
