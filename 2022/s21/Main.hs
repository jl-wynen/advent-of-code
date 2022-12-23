import           FileInput (parseInput, Monkey (..), Op (..), Operation (..))
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

type Table = Map.Map String Operation

toTable :: [Monkey] -> Table
toTable = Map.fromList . map (\(Monkey out op) -> (out, op))

evalOp :: Op -> Int -> Int -> Int
evalOp Plus a b = a + b
evalOp Minus a b = a - b
evalOp Times a b = a * b
evalOp Divides a b = a `div` b
evalOp Equals _ _ = error "Cannot eval equals"

eval :: String -> Table -> Int
eval name table = case table Map.! name of
                    Num i -> i
                    Operation a b op -> evalOp op (eval a table) (eval b table)
                    Input -> error "Cannot eval input"

elemIsNum :: String -> Table -> Bool
elemIsNum name table = case table Map.! name of
                        Num _ -> True
                        _ -> False

foldConstants :: Table -> Table
foldConstants table = let (changed, newTable) = Map.mapAccum kernel False table
                      in if changed then foldConstants newTable else newTable 
    where kernel accum orig@(Operation a b op) =
                case (table Map.! a, table Map.! b) of
                    (Num i, Num j) -> (True, Num $ evalOp op i j)
                    _ -> (accum, orig)
          kernel accum (Num i) = (accum, (Num i))
          kernel accum Input = (accum, Input)

replaceForPart2 :: Table -> Table
replaceForPart2 = Map.adjust adjustRoot "root"
                . Map.adjust adjustInput "humn" 
    where adjustRoot (Operation a b _) = Operation a b Equals
          adjustRoot _ = error "bad root"
          adjustInput (Num _) = Input
          adjustInput _ = error "bad root"

ensureEachUsedOnce :: Table -> ()
ensureEachUsedOnce table =
    let list = Map.toList table
    in if any (>1) $ map (\(k, _) -> length $ filter (\(_, v) -> contains k v) list) list
       then error "monkey used more than once"
       else ()
    where contains key (Operation a b _) = key == a || key == b
          contains _ _ = False

solveForFirst :: String -> String -> Op -> Operation
solveForFirst out b Plus = Operation out b Minus
solveForFirst out b Minus = Operation out b Plus
solveForFirst out b Times = Operation out b Divides
solveForFirst out b Divides = Operation out b Times
solveForFirst _ _ Equals = error "cannot invert equals"

solveForSecond :: String -> String -> Op -> Operation
solveForSecond out a Plus = Operation out a Minus
solveForSecond out a Minus = Operation a out Minus
solveForSecond out a Times = Operation out a Divides
solveForSecond out a Divides = Operation a out Divides
solveForSecond _ _ Equals = error "cannot invert equals"

invert :: Table -> Table
invert table = Map.fromList $ map kernel $ filter (\(k, _) -> k /= "humn") $ Map.toList table
    where kernel ("root", (Operation a b Equals)) =
            if elemIsNum a table
            then (b, table Map.! a)
            else (a, table Map.! b)
          kernel (out, (Operation a b op)) =
            if elemIsNum a table
            then (a, solveForSecond out b op)
            else (b, solveForFirst out a op)
          kernel x = x

drawOp :: Op -> String
drawOp Plus = "+"
drawOp Minus = "-"
drawOp Times = "*"
drawOp Divides = "/"
drawOp Equals = "="

draw :: Table -> String
draw table = intercalate "\n" $ map kernel $ Map.toList table
    where kernel (out, (Operation a b op)) = out ++ ": " ++ a ++ (drawOp op) ++ b
          kernel (out, (Num i)) = out ++ ": " ++ (show i)
          kernel (out, Input) = out ++ ": Input"

main :: IO ()
main = do
    monkeyList <- parseInput
    let monkeys = toTable monkeyList
    putStrLn $ "Part 1: " ++ (show $ eval "root" monkeys)

    let monkeys2 = replaceForPart2 monkeys
        folded = foldConstants monkeys2
        _ = ensureEachUsedOnce folded
    putStrLn $ draw folded
    putStrLn "------"
    putStrLn $ draw $ invert folded

