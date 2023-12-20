import System.Environment (getArgs)
import Data.Map as M (Map, empty, insert, (!), lookup, elems, member)
import Data.List.Split (splitOn)
import Data.Sequence as S (Seq ((:|>), Empty), Seq ((:<|)), empty, (><), fromList)
import Debug.Trace (trace)

type Name = String
data Pulse = Hi | Lo deriving (Show, Eq)
type ConInp = Map Name Pulse
type Out = [Name]
data State = On | Off deriving (Show)
data Module = B Name Out | C Name ConInp Out | F Name State Out deriving (Show)
type Modules = Map Name Module
type Count = (Int, Int)

mName :: Module -> Name
mName (B n _) = n
mName (C n _ _) = n
mName (F n _ _) = n

mOuts :: Module -> Out
mOuts (B _ o) = o
mOuts (C _ _ o) = o
mOuts (F _ _ o) = o

-- TODO: This should be a record field or whatever it's called
cInp :: Module -> ConInp
cInp (C _ i _) = i
cInp _ = error "Called cInp with wrong module"

initModule :: String -> Module
initModule s = case head name of
  '%' -> F (tail name) Off out
  '&' -> C (tail name) M.empty out
  _   -> B name out
  where
    spl = splitOn " -> " s
    [name, outs] = [head spl, last spl]
    out = splitOn ", " outs

addToInp :: Modules -> Name -> Name -> Modules
addToInp m cName iName = case cName `M.lookup` m of
  Just (C n inp out) -> insert n updatedC m
    where
      updatedC = C n (insert iName Lo inp) out
  _ -> m

updateConInp :: Modules -> String -> Modules
updateConInp m s = foldl (\acc n -> addToInp acc n name) m outs
  where
    md = initModule s
    name = mName md
    outs = mOuts md

parseModulesInit :: [String] -> Modules
parseModulesInit ss = go ss M.empty
  where
    go [] m = m
    go (x:xs) m = let md = initModule x in go xs $ insert (mName md) md m

flipState :: State -> State
flipState On = Off
flipState Off = On

stateToPulse :: State -> Pulse
stateToPulse On = Hi
stateToPulse Off = Lo

updateConState :: Module -> (Name, Pulse) -> Module
updateConState (C n i os) (f, p) = C n (insert f p i) os
updateConState m _ = m

apply :: Module -> (Name, Pulse) -> (Module, [(Name, Pulse)])
apply m@(B _ os) (_, p)   = (m, [(o, p) | o <- os])
apply m@(F {}) (_, Hi)    = (m, [])
apply (F n s os) (_, Lo)  = (F n (flipState s) os, [(o, stateToPulse (flipState s)) | o <- os])
apply m@(C _ _ os) (f, p) = (newC, [(o, newCPulse) | o <- os])
  where
    newC = updateConState m (f, p)
    newCPulse = if all (==Hi) (elems (cInp newC)) then Lo else Hi

buttonRec :: Modules -> Count -> Seq (Name, Name, Pulse) -> (Modules, Count)
buttonRec ms c Empty = (ms, c)
buttonRec ms (lo, hi) ((f, t, p) :<| ss) = if t `member` ms then buttonRec newMs newC newS else buttonRec ms newC ss
  where
    newC = if p == Hi then (lo, hi + 1) else (lo + 1, hi)
    (newM, ps) = apply (ms!t) (f, p)
    newMs = insert t newM ms
    newS = ss <> fromList [(t, newT, newP) | (newT, newP) <- ps]

button :: (Modules, Count) -> (Modules, Count)
button (m, c) = buttonRec m c (S.empty :|> ("button", "broadcaster", Lo))

pulseScore :: Count -> Int
pulseScore (lo, hi) = lo * hi

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args else "inputs/day20/input.txt"
    let initModules = parseModulesInit $ lines contents
    let modules = foldl updateConInp initModules $ lines contents
    print $ elems modules
    let (resultM, resultC) = last $ take 1001 $ iterate button (modules, (0, 0))
    print $ pulseScore resultC
    -- let (newModules, count) = button modules (0, 0)
    -- print $ elems newModules
    -- print count
    -- let (newerModules, count) = button newModules (0, 0)
    -- print $ elems newerModules
    -- print count
    -- let (newererModules, count) = button newerModules (0, 0)
    -- print $ elems newererModules
    -- print count