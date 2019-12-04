import System.IO
import Debug.Trace
import Utility as U

sol1 :: String -> Int
sol1 _ = 0

sol2 :: String -> Int
sol2 _ = 0

main :: IO ()
main = do
        handle <- openFile "input0.txt" ReadMode
        contents <- hGetContents handle
        print $ sol1 contents
        print $ sol2 contents