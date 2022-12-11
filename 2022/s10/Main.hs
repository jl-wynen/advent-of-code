import           FileInput (Instruction (..), parseInput)

instructionLength :: Instruction -> Int
instructionLength Noop     = 1
instructionLength (Addx _) = 2

padCycles :: [Instruction] -> [Instruction]
padCycles = foldr (\x acc -> (replicate (instructionLength x - 1) Noop) ++ x:acc) []

eval :: Instruction -> Int -> Int
eval Noop reg     = reg
eval (Addx x) reg = x + reg

execute :: [Instruction] -> Int -> [Int]
execute [] _ = []
execute (inst:remainder) register =
    register : execute remainder (eval inst register)

exe :: [Instruction] -> [Int]
exe instructions = execute instructions 1

fillCRT :: [Int] -> Int -> [Char]
fillCRT [] _ = []
fillCRT (reg:registers) cyc
    | (cyc `mod` 40) == 0 = '\n':pixel:fillCRT registers (cyc+1)
    | otherwise = pixel : fillCRT registers (cyc+1)
    where pixel = if (abs (reg - cyc`mod`40)) < 2
                  then '#'
                  else '.'

getMany :: [Int] -> [a] -> [a]
getMany indices xs = [x | (i, x) <- zip [1..] xs, i `elem` indices]

main :: IO ()
main = do
    instructions <- parseInput >>= return . padCycles
    let toInspect = [20, 60..220]
        registerValues = exe instructions
        signalStrengths = getMany toInspect $ zipWith (*) [1..] registerValues
    putStrLn $ "Part 1: " ++ (show $ sum signalStrengths)
    putStrLn $ "Part 2: " ++ fillCRT registerValues 0
