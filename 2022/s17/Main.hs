import           Data.List (intersect, intercalate)
import           FileInput (readInput)

-- (x, y)
type Rock = [(Int, Int)]
type Pos = (Int, Int)

type Jet = [Int]

data RockStack = RockStack Int Int [Rock]

rockShapes :: [Rock]
rockShapes = [ [(0, 0), (1, 0), (2, 0), (3, 0)]
             , [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
             , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
             , [(0, 0), (0, 1), (0, 2), (0, 3)]
             , [(0, 0), (1, 0), (0, 1), (1, 1)]
             ]

moveTo :: Pos -> Rock -> Rock
moveTo (x, y) rock = map (\(rx, ry) -> (rx + x, ry + y)) rock

overlaps :: Rock -> Rock -> Bool
overlaps a b = not $ null $ intersect a b

overlapsStack :: RockStack -> Rock -> Bool
overlapsStack (RockStack _ _ rocks) rock = any (overlaps rock) rocks

outOfBounds :: Int -> Rock -> Bool
outOfBounds width rock = any (\(x, _) -> x < 0 || x >= width) rock

belowBottom :: Rock -> Bool
belowBottom rock = any ((<0) . snd) rock

spawnPos :: RockStack -> Pos
spawnPos (RockStack h _ _) = (2, h + 3)

dropRock :: Int -> Jet -> RockStack -> [Rock] -> Rock -> RockStack
dropRock width jet stack rocks rock
    | belowBottom moved || overlapsStack stack moved =
        dropNextRock rock width jet stack rocks
    | otherwise =
        shiftRock width jet stack rocks moved
    where moved = map (\(x, y) -> (x, y-1)) rock

shiftRock :: Int -> Jet -> RockStack -> [Rock] -> Rock -> RockStack
shiftRock _ [] _ _ _ = error "jet is empty"
shiftRock width (d:jet) stack rocks rock =
    let  moved = map (\(x, y) -> (x+d, y)) rock
         next = if outOfBounds width moved || overlapsStack stack moved
                then rock
                else moved
    in dropRock width jet stack rocks next

newHeight :: Int -> Rock -> Int
newHeight oldHeight rock =
    max oldHeight $ (1 + (maximum $ map snd rock))

addToStack :: Rock -> RockStack -> RockStack
addToStack rock (RockStack h n stack) =
    (RockStack (newHeight h rock) (n+1) (rock:stack))

dropNextRock :: Rock -> Int -> Jet -> RockStack -> [Rock] -> RockStack
dropNextRock rock _ _ stack [] = addToStack rock stack
dropNextRock rock width jet stack (next:rocks) =
    let newStack = addToStack rock stack
        newRock = moveTo (spawnPos newStack) next
    in shiftRock width jet newStack rocks newRock

dropRocks :: Int -> Jet -> [Rock] -> RockStack
dropRocks width jet rocks =
    let initialStack = (RockStack 0 0 [])
        firstRock = moveTo (spawnPos initialStack) $ head rocks
    in shiftRock width jet initialStack (tail rocks) firstRock

draw :: RockStack -> String
draw (RockStack h _ stack) =
    intercalate "\n" [[if any (\rock -> (x,y) `elem` rock) stack then '#' else '.' 
                      | x <- [0..6]]
                     | y <- [(h+3),(h+2)..(0)]]

main :: IO ()
main = do
    input <- readInput
    let jet = cycle $ map (\c -> if c == '<' then -1 else 1) input
        width = 7
        rocks = take 2022 $ cycle rockShapes
        s@(RockStack h _ _) = dropRocks width jet rocks
    -- putStrLn $ draw s
    putStrLn $ "Part 1: height of tower = " ++ show h
