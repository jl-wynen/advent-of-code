import Paths_s02

{-
part 1:
A, X = rock
B, Y = paper
C, Z = scissors

part 2:
X = lose
Y = draw
Z = win
-}

readInput :: IO (String)
readInput = do
    inputFile <- getDataFileName "input"
    readFile inputFile

toPair :: [String] -> (Char, Char)
toPair [a, b] = (head a, head b)
toPair _ = error "bad argument"

parseInput :: String -> [(Char, Char)]
parseInput s = map toPair $ map words $ lines s

scoreForShape :: Char -> Int
scoreForShape 'A' = 1
scoreForShape 'B' = 2
scoreForShape 'C' = 3
scoreForShape _ = error "bad argument"

part1Translate :: Char -> Char
part1Translate 'X' = 'A'
part1Translate 'Y' = 'B'
part1Translate 'Z' = 'C'
part1Translate _ = error "bad argument"

data Outcome = Lose | Draw | Win

scoreForOutcome :: Outcome -> Int
scoreForOutcome Lose = 0
scoreForOutcome Draw = 3
scoreForOutcome Win = 6

outcome :: (Char, Char) -> Outcome
outcome ('A', 'B') = Win
outcome ('B', 'C') = Win
outcome ('C', 'A') = Win
outcome ('A', 'C') = Lose
outcome ('B', 'A') = Lose
outcome ('C', 'B') = Lose
outcome _ = Draw

desiredShape :: (Char, Char) -> Char
desiredShape ('A', 'X') = 'C'
desiredShape ('B', 'X') = 'A'
desiredShape ('C', 'X') = 'B'
desiredShape ('A', 'Z') = 'B'
desiredShape ('B', 'Z') = 'C'
desiredShape ('C', 'Z') = 'A'
desiredShape (opponend, 'Y') = opponend
desiredShape _ = error "bad argument"

calculateScore :: (Char, Char) -> Int
calculateScore round'@(_, me) = (scoreForShape me) + (scoreForOutcome $ outcome round')


main :: IO ()
main = do
    input <- readInput
    let plan = parseInput input
        part1Rounds = map (\(a, b) -> (a, part1Translate b)) plan
        part2Rounds = map (\(a, b) -> (a, desiredShape (a, b))) plan
        part1Score = sum $ map calculateScore part1Rounds
        part2Score = sum $ map calculateScore part2Rounds
    putStr "Part 1: score="
    print part1Score
    putStr "Part 2: score="
    print part2Score
