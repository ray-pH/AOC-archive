import System.IO

data Sys = File String (Maybe Integer)
         | Dir String (Maybe Integer) [Sys]
         deriving Show

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy e arr
    | elem e arr = larr : splitBy e rarr
    | otherwise  = [arr]
    where (larr, _:rarr) = span (/= e) arr

isFile :: Sys -> Bool
isFile (File _ _)  = True
isFile (Dir _ _ _) = False
isDir = not . isFile

mkdir :: String -> Sys -> Sys
mkdir dirname (Dir str size content) = Dir str size ((Dir dirname Nothing []):content)

mkfile :: String -> Integer -> Sys -> Sys
mkfile filename filesize (Dir str size content) = Dir str size ((File filename (Just filesize)):content)

getDirContentId :: String -> [Sys] -> Int
getDirContentId name content = id
    where zipped  = zip [0..] content
          dironly = filter (\(_,cont) -> isDir cont) zipped
          correctname = filter (\(_,Dir dirname _ _) -> dirname == name ) dironly
          (id,nam) = head correctname

mkdirtraverse :: [String] -> String -> Sys -> Sys
mkdirtraverse [] dirname sys = mkdir dirname sys
mkdirtraverse (x:xs) dirname (Dir str size content) = Dir str size content'
    where content' = prev ++ [dirx'] ++ next
          dirid = getDirContentId x content
          prev  = take (dirid  ) content
          next  = drop (dirid+1) content
          dirx  = content !! dirid
          dirx' = mkdirtraverse xs dirname dirx

mkfiletraverse :: [String] -> String -> Integer -> Sys -> Sys
mkfiletraverse [] filename filesize sys = mkfile filename filesize sys
mkfiletraverse (x:xs) filename filesize (Dir str size content) = Dir str size content'
    where content' = prev ++ [dirx'] ++ next
          dirid = getDirContentId x content
          prev  = take (dirid  ) content
          next  = drop (dirid+1) content
          dirx  = content !! dirid
          dirx' = mkfiletraverse xs filename filesize dirx

applyLs :: [String] -> [String] -> Sys -> Sys
applyLs [] _ sys = sys
applyLs (x:xs) dirstack sys
  | "dir " == take 4 x = mkdirtraverse dirstack dirname $ applyLs xs dirstack sys
  | otherwise          = mkfiletraverse dirstack filename filesize $ applyLs xs dirstack sys
  where dirname = drop 4 x
        [sizestr, filename] = splitBy ' ' x
        filesize = read sizestr

runCommands :: [[String]] -> [String] -> Sys -> Sys
runCommands [] _ sys = sys
runCommands (com:coms) dirstack sys
  | isLs                 = runCommands coms dirstack $ applyLs (tail com) dirstack sys
  | isCd && cdto == ".." = runCommands coms (init dirstack) sys
  | isCd && cdto /= ".." = runCommands coms (dirstack ++ [cdto]) sys
  where header = com !! 0
        isLs   = header == "ls"
        isCd   = take 3 header == "cd "
        [_,cdto] = splitBy ' ' header

-- allFiles :: [Sys] -> Bool
-- allFiles = or . map isFile

allEvaluated :: [Sys] -> Bool
allEvaluated [] = True
allEvaluated ((File _ _):xs) = allEvaluated xs
allEvaluated ((Dir _ (Just _) _):xs) = allEvaluated xs
allEvaluated ((Dir _ Nothing _):_) = False

getSize :: [Sys] -> [Integer]
getSize [] = []
getSize ((Dir _ (Just size) _):xs) = size : getSize xs
getSize ((File _ (Just size)):xs)  = size : getSize xs

calcDirSize :: Sys -> Sys
calcDirSize (File name size) = File name size
calcDirSize (Dir name size content)
    | allEvaluated content = Dir name (Just (sum $ getSize content)) content
    | otherwise    = calcDirSize (Dir name size content')
    where content' = map calcDirSize content

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

collectAllDirSizes :: Sys -> [Integer]
collectAllDirSizes (File _ _) = []
collectAllDirSizes (Dir name (Just size) content) = size : (flatten $ map collectAllDirSizes content)

main :: IO()
main = do
    -- file <- openFile "inpex.txt" ReadMode
    -- file <- openFile "inpe.txt" ReadMode
    file <- openFile "input.txt" ReadMode
    cont <- hGetContents file
    let rawcommands = splitBy '$' cont
        commands  = map lines $ map tail $ drop 2 rawcommands
        root = Dir "/" Nothing [] :: Sys
        root' = runCommands commands [] root
        root'' = calcDirSize root'
        dirsizes = collectAllDirSizes root''
        smalldirsizes = filter (\x -> x <= 100000) dirsizes
    -- print commands
    -- print root
    print root''
    -- print dirsizes
    print $ sum smalldirsizes
