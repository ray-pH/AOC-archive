module Main where

import System.IO

data Tile = Tile { idT :: Int
                 , top :: String
                 , bot :: String
                 , lef :: String
                 , rig :: String
                 } deriving Show

instance Eq Tile where
    (==) t1 t2 = idT t1 == idT t2

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

toTile :: [String] -> Tile
toTile (x:xs) = let theid = read $ init $ last $ words x
                    top = head xs
                    bot = last xs
                    lef = map head xs 
                    rig = map last xs
                 in Tile theid top bot lef rig

getSide :: Tile -> [String]
getSide tile = [t,b,l,r] where Tile _ t b l r = tile

sideCompatible :: String -> String -> Bool
sideCompatible a b = (a == b) || (a == reverse b)

tileCompatible :: Tile -> Tile -> Bool
tileCompatible tile tile' = or [sideCompatible s s' | s<-sides, s'<-sides' ]
            where sides = getSide tile
                  sides' = getSide tile'

whichCompatible :: [Tile] -> Tile -> [Int]
whichCompatible tiles tile = [idT t | t <- tiles, tileCompatible tile t, t /= tile]

main :: IO()
main = do
    file <- openFile "20-input.txt" ReadMode
    -- file <- openFile "20-ex.txt" ReadMode
    cont <- hGetContents file
    let l = lines cont
        rawtiles = split "" l
        tiles = map toTile rawtiles
        compats = zip (map idT tiles) $ map (whichCompatible tiles) tiles
        corners = [ i | (i,tls) <- compats, length tls == 2 ]
    mapM_ print tiles
    print $ length tiles
    mapM_ print compats
    print corners
    print $ product corners
