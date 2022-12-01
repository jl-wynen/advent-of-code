import Paths_s01
import Data.List (sortOn)

readInput :: IO (String)
readInput = do
    inputFile <- getDataFileName "input"
    readFile inputFile


dropFirst :: [String] -> [String]
dropFirst [] = []
dropFirst (_:t) = t


groupElves :: [String] -> [[String]]
groupElves [] = []
groupElves l =
    let (a, b) = break (=="") l
    in a : (groupElves $ dropFirst b)

toInt :: String -> Int
toInt s = read s

elveCalories :: String -> [[Int]]
elveCalories x = map (map toInt) (groupElves $ lines x)

main :: IO ()
main = do
    input <- readInput
    let calories = map sum $ elveCalories input
    putStr "Part 1: maximum calories  = "
    print $ maximum $ calories
    putStr "Part 2: calories of top 3 = "
    print $ sum $ take 3 $ sortOn negate calories
