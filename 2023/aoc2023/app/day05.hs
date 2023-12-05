import Control.Exception (throwIO)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.List (sortBy)

data AlmanacRange = AR {
  destination :: Integer,
  source :: Integer,
  rlength :: Integer
} deriving (Show)
type Seed = Integer
data SeedRange = SR {
  start :: Integer,
  slength :: Integer
} deriving (Show)
data RangeLength = Bounded Integer | Unbounded deriving (Show)
type AlmanacMap = [AlmanacRange]
data AlmanacRangeExtended = ARE {
  dst :: Integer,
  src :: Integer,
  range :: RangeLength
} deriving (Show)
type AlmanacMapExtended = [AlmanacRangeExtended]

parseAlmanacRange :: [Integer] -> IO AlmanacRange
parseAlmanacRange [x, y, z] = pure $ AR x y z
parseAlmanacRange _ = throwIO $ userError "Wrong list length for AlmanacRange"

parseAlmanacMap :: [String] -> IO AlmanacMap
parseAlmanacMap ss = mapM parseAlmanacRange ints
  where
    ints = map (map (read :: String -> Integer) . words) (tail ss)

parseSeedRanges :: [Integer] -> [SeedRange] -> IO [SeedRange]
parseSeedRanges (a:b:rs) curr = parseSeedRanges rs (SR a b : curr)
parseSeedRanges [] curr = pure curr
parseSeedRanges [_] _ = throwIO $ userError "Could not parse seed ranges"

inAlmanacRange :: Seed -> AlmanacRange -> Maybe Seed
inAlmanacRange s a = if s > source a && s - source a < rlength a
  then Just (destination a + (s - source a))
  else Nothing

convertSeed :: Seed -> AlmanacMap -> Seed
convertSeed s (a:as) = case inAlmanacRange s a of
  Just seed -> seed
  Nothing   -> convertSeed s as
convertSeed s [] = s

performAllConvertions :: Seed -> [AlmanacMap] -> Seed
performAllConvertions = foldl convertSeed

-- part 2
extendAlmanacRange :: AlmanacRange -> AlmanacRangeExtended
extendAlmanacRange (AR d s r) = ARE d s (Bounded r)

extendSortedAlmanacMapRec :: AlmanacMap -> AlmanacMapExtended
extendSortedAlmanacMapRec [] = []
extendSortedAlmanacMapRec [AR d s r] = [ARE d s (Bounded r), ARE (s + r) (s + r) Unbounded]
extendSortedAlmanacMapRec (ar1:ar2:am) = if s1 + r1 == s2
  then extendAlmanacRange ar1 : extendSortedAlmanacMapRec (ar2:am)
  else extendAlmanacRange ar1 : ARE (s1 + r1) (s1 + r1) (Bounded (s2 - s1 - r1)) : extendSortedAlmanacMapRec (ar2:am)
  where
    AR _ s1 r1 = ar1
    AR _ s2 _ = ar2

extendSortedAlmanacMap :: AlmanacMap -> AlmanacMapExtended
extendSortedAlmanacMap [] = []
extendSortedAlmanacMap ((AR d 0 r):am) = ARE d 0 (Bounded r) : extendSortedAlmanacMapRec am
extendSortedAlmanacMap (ar:am) = ARE 0 0 (Bounded (source ar)) : extendSortedAlmanacMapRec (ar:am)

extendAlmanacMap :: AlmanacMap -> AlmanacMapExtended
extendAlmanacMap am = extendSortedAlmanacMap sortedAlmanachMap
  where
    sortedAlmanachMap = sortBy (\ar1 ar2 -> compare (source ar1) (source ar2)) am

convertSeedRange :: SeedRange -> AlmanacMapExtended -> [SeedRange]
convertSeedRange (SR s sl) ((ARE ad as (Bounded r)):am)
  | seedRangeStart >= almanacRangeStart && seedRangeEnd <= almanacRangeEnd  = [SR (ad + (s - as)) sl]
  | seedRangeStart >= almanacRangeEnd = convertSeedRange (SR s sl) am
  | seedRangeStart >= almanacRangeStart = SR (ad + (s - as)) (as + r - s) : convertSeedRange (SR (as + r) (sl - (r + as - s))) am
  -- This should never happen
  | s < as = []
  where
    seedRangeStart = s
    seedRangeEnd = s + sl
    almanacRangeStart = as
    almanacRangeEnd = as + r
convertSeedRange (SR s sl) ((ARE _ _ Unbounded):_) = [SR s sl]
convertSeedRange _ [] = []
convertSeedRange (SR _ _) ((ARE _ _ (Bounded _)):_) = []

convertSeedRanges :: [SeedRange] -> AlmanacMapExtended -> [SeedRange]
convertSeedRanges srs ame = concatMap (`convertSeedRange` ame) srs

convertSeedRangesFully :: [SeedRange] -> [AlmanacMapExtended] -> [SeedRange]
convertSeedRangesFully = foldl convertSeedRanges

main :: IO ()
main =
  do
    args <- getArgs
    contents <- readFile $ if not (null args) then head args  else "inputs/day05/input.txt"
    let groups = map lines $ splitOn "\n\n" contents
    almanacMaps <- mapM parseAlmanacMap (tail groups)
    let seeds = (map (read :: String -> Integer) . words . last . splitOn ": ") (head (head groups))
    let locations = map (`performAllConvertions` almanacMaps) seeds
    print $ minimum locations
    srs <- parseSeedRanges seeds []
    print $ minimum $ map start $ convertSeedRangesFully srs (map extendAlmanacMap almanacMaps)