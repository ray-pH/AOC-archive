import System.IO
import Data.List

main :: IO()
main = do
    let arr = [[2],[4],[2,3],[1,2,4]]
        -- sorted  = sort arr
        sorted  = sortBy (\x y -> compare (length x) (length y)) arr
    print sorted
