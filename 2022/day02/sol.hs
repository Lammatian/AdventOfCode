data TheirChoice = A | B | C deriving(Show, Read)
data MyChoice = X | Y | Z deriving(Show, Read)
data Round = Round {
  theirChoice :: TheirChoice,
  myChoice :: MyChoice
} deriving(Show)

choiceScore :: Round -> Int
choiceScore (Round _ X) = 1
choiceScore (Round _ Y) = 2
choiceScore (Round _ Z) = 3

roundScore :: Round -> Int
roundScore (Round A X) = 3
roundScore (Round A Y) = 6
roundScore (Round A Z) = 0
roundScore (Round B X) = 0
roundScore (Round B Y) = 3
roundScore (Round B Z) = 6
roundScore (Round C X) = 6
roundScore (Round C Y) = 0
roundScore (Round C Z) = 3

main :: IO()
main =
  do
    contents <- readFile "../inputs/day02/input"
    let rounds = map words $ lines contents
    let parsedRounds = map (\[t, m] -> Round (read t :: TheirChoice) (read m :: MyChoice)) rounds
    print $ sum $ map (\r -> roundScore r + choiceScore r) parsedRounds