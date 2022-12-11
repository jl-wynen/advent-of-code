module FileInput
( parseInput
) where

import Data.List.Split (splitOn)
import Monkey (Monkey (..), Operation, Target, Worry)
import Paths_s11

readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

dropUntil :: Char -> String -> String
dropUntil delim = tail . dropWhile (/= delim)

parseStartingWorry :: String -> [Worry]
parseStartingWorry = map read . splitOn ", " . dropUntil ':'

parseOperation :: String -> Operation
parseOperation line  = case funcSpec of
    ["old", "*", "old"] -> (^ (2 :: Int))
    ["old", "+", x] -> (+ (read x))
    ["old", "*", x] -> (* (read x))
    _ -> error "bad operation"
    where funcSpec = words $ dropUntil '=' line

parseThrow :: String -> String -> String -> (Int, Target)
parseThrow test trueThrow falseThrow = 
    (divisor,
     (\worry -> if worry `rem` divisor == 0 then trueTarget else falseTarget))
    where divisor = parseTrailingInt test
          trueTarget = parseTrailingInt trueThrow
          falseTarget = parseTrailingInt falseThrow

parseMonkey :: [String] -> ([Worry], Int, Monkey)
parseMonkey 
    [index, start, operation, test, trueThrow, falseThrow] =
        let (divisor, target) = parseThrow test trueThrow falseThrow
        in
        (parseStartingWorry start, 
         divisor,
         Monkey
            (parseTrailingInt $ takeWhile (/=':') index)
            (parseOperation operation)
            target)
parseMonkey _ = error "bad monkey"

parseTrailingInt :: String -> Int
parseTrailingInt =  read . last . words

parseInput :: IO ([[Worry]], [Int], [Monkey])
parseInput = do 
    readInput (unzip3 . map (parseMonkey . lines) . splitOn "\n\n") >>= return
