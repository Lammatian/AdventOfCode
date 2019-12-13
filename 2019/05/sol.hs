module Main where

import System.Environment
import System.IO
import Debug.Trace
import Data.Sequence as Seq
import Data.Maybe

import Utility as U

data Mode        = Position | Value | Undefined
data Instruction = 
      Add Mode Mode
    | Mul Mode Mode
    | Inp
    | Out Mode
    | End

sol1 :: String -> Int
sol1 s = result
        where str_list = U.splitOn ',' s
              int_list = map U.readInt str_list
              int_seq  = fromList int_list
              result   = simulate int_seq

sol1input :: Int
sol1input = 1

get :: Int -> Seq Int -> Int
get i s = fromMaybe 0 $ Seq.lookup i s

initialise :: Seq Int -> Int -> Int -> Seq Int
initialise s a b = update 1 a $ update 2 b s

simulate :: Seq Int -> Int
simulate s = fromMaybe 0 $ Seq.lookup 0 $ simRec 0 s

simRec :: Int -> Seq Int -> Seq Int
simRec curr s = case parseInstruction $ get curr s of
                    Add m1 m2 -> simRec (curr + 4) $ performArithmetic (+) m1 m2 curr s
                    Mul m1 m2 -> simRec (curr + 4) $ performArithmetic (*) m1 m2 curr s
                    Inp       -> simRec (curr + 2) $ update a sol1input s
                        where
                            a = get (curr + 1) s
                    Out m1    -> simRec (traceShowId a - a + curr + 2) s
                        where
                            a = retrieve (get (curr + 1) s) m1 s
                    End       -> s

retrieve :: Int -> Mode -> Seq Int -> Int
retrieve i Value s    = i
retrieve i Position s = fromMaybe 0 $ Seq.lookup i s

parseInstruction :: Int -> Instruction
parseInstruction i = case i `mod` 10 of
                        1 -> Add (intToMode (digit i 2)) (intToMode (digit i 3))
                        2 -> Mul (intToMode (digit i 2)) (intToMode (digit i 3))
                        3 -> Inp
                        4 -> Out (intToMode (digit i 2))
                        9 -> End

performArithmetic :: (Int -> Int -> Int) -> Mode -> Mode -> Int -> Seq Int -> Seq Int
performArithmetic op m1 m2 curr s = update c (a `op` b) s
    where
        a = retrieve (get (curr + 1) s) m1 s
        b = retrieve (get (curr + 2) s) m2 s
        c = get (curr + 3) s


intToMode :: Int -> Mode
intToMode 0 = Position
intToMode 1 = Value
intToMode _ = Undefined

digit :: Int -> Int -> Int
digit i n = (i `div` 10^n) `mod` 10

sol2 :: String -> Int
sol2 _ = 0

main :: IO ()
main = do
        [filename] <- getArgs
        handle     <- openFile filename ReadMode
        contents   <- hGetContents handle
        print $ sol1 contents
        print $ sol2 contents