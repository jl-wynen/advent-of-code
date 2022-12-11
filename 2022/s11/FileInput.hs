module FileInput
( parseInput
) where

import Data.List.Split (splitOn)
import Monkey (Monkey (..), Operation, Target)
import Paths_s11

readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

dropUntil :: Char -> String -> String
dropUntil delim = tail . dropWhile (/= delim)

parseStartingWorry :: String -> [Int]
parseStartingWorry = map read . splitOn ", " . dropUntil ':'

parseOperation :: String -> Operation
parseOperation line  = case funcSpec of
    ["old", "*", "old"] -> (^ (2 :: Int))
    ["old", "+", x] -> (+ (read x))
    ["old", "*", x] -> (* (read x))
    _ -> error "bad operation"
    where funcSpec = words $ dropUntil '=' line

parseThrow :: String -> String -> String -> Target
parseThrow test trueThrow falseThrow = 
    (\worry -> if worry `rem` divisor == 0 then trueTarget else falseTarget)
    where divisor = parseTrailingInt test
          trueTarget = parseTrailingInt trueThrow
          falseTarget = parseTrailingInt falseThrow

parseMonkey :: [String] -> ([Int], Monkey)
parseMonkey 
    [index, start, operation, test, trueThrow, falseThrow] =
        (parseStartingWorry start, 
         Monkey
            (parseTrailingInt $ takeWhile (/=':') index)
            (parseOperation operation)
            (parseThrow test trueThrow falseThrow))
parseMonkey _ = error "bad monkey"

parseTrailingInt :: String -> Int
parseTrailingInt =  read . last . words

parseInput :: IO ([[Int]], [Monkey])
parseInput = do 
    readInput (unzip . map (parseMonkey . lines) . splitOn "\n\n") >>= return
