import           Data.List (transpose)
import           Paths_s05

readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

parseItem :: [Char] -> Maybe Char
parseItem ('[':c:']':_) = Just c
parseItem _             = Nothing

parseLayer :: [Char] -> [Maybe Char]
parseLayer [] = []
parseLayer layer = parseItem h : parseLayer t
    where (h,t) = splitAt 4 layer

cropStack :: [Maybe Char] -> [Char]
cropStack stack = [c | Just c <- stack]

parseStackSection :: [String] -> [[Char]]
parseStackSection = map cropStack . transpose . map parseLayer

data Move = Move {amount      :: Int
                 ,source      :: Int
                 ,destination :: Int
                 } deriving (Show)

parseMove :: String -> Move
parseMove line = case words line of
                   ["move", a, "from", s, "to", d] -> Move (read a) ((read s)-1) ((read d)-1)
                   _ -> error "bad move line"

parseMoves :: [String] -> [Move]
parseMoves moves = map parseMove moves

doMove :: ([Char] -> [Char]) ->Move -> [[Char]] -> [[Char]]
doMove processPicked move stacks =
    let toMove = processPicked $ take (amount move) $ stacks !! (source move)
    in [if i == (source move)
        then drop (amount move) stack
        else if i == (destination move)
        then toMove ++ stack else stack
        | (i, stack) <- zip [0..] stacks]

doMoves :: ([Char] -> [Char]) -> [Move] ->  [[Char]] -> [[Char]]
doMoves _ [] stacks          = stacks
doMoves processPicked (move:rest) stacks = doMoves processPicked rest $ doMove processPicked move stacks

main :: IO ()
main = do
    (initialStacks, moves) <- readInput ((\(i, m) -> (parseStackSection i, parseMoves $ tail m)) . break (=="") . lines)
    putStr "Part 1: Heads of stacks = "
    print $ map head $ doMoves reverse moves initialStacks
    putStr "Part 2: Heads of stacks = "
    print $ map head $ doMoves id moves initialStacks
