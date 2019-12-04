import System.IO
import Data.Sequence as Seq
import Data.Maybe
import Debug.Trace
import Utility as U

sol1 :: String -> Integer
sol1 s = result 
        where str_list = U.splitOn ',' s
              int_list = map U.readInteger str_list
              int_seq  = fromList int_list
              seq_1202 = initialise int_seq 12 2
              result   = simulate seq_1202

get :: Integer -> Seq Integer -> Integer
get i s = fromMaybe 0 $ Seq.lookup (fromIntegral i) s

initialise :: Seq Integer -> Integer -> Integer -> Seq Integer
initialise s a b = update 1 a $ update 2 b s

simulate :: Seq Integer -> Integer
simulate s = fromMaybe 0 $ Seq.lookup 0 $ simRec 0 s

simRec :: Integer -> Seq Integer -> Seq Integer
simRec curr s = case get curr s of
                    1  -> simRec (curr + 4) $ update (fromIntegral c) (a + b) s
                    2  -> simRec (curr + 4) $ update (fromIntegral c) (a * b) s
                    99 -> s
                    _  -> s
                where
                    a = get (get (curr + 1) s) s
                    b = get (get (curr + 2) s) s
                    c = get (curr + 3) s

sol2 :: String -> Integer
sol2 s = sol2rec int_seq 0 0
        where str_list = U.splitOn ',' s
              int_list = map U.readInteger str_list
              int_seq  = fromList int_list

sol2rec :: Seq Integer -> Integer -> Integer -> Integer
sol2rec s a b 
    | simulate (initialise s a b) == 19690720 = 100 * a + b
    | a == 99 && b == 99                      = 9999
    | b == 99                                 = sol2rec s (a + 1) 0
    | otherwise                               = sol2rec s a (b + 1)

main :: IO ()
main = do
        handle <- openFile "input0.txt" ReadMode
        contents <- hGetContents handle
        print $ sol1 contents
        print $ sol2 contents