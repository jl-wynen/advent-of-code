import Paths_s{day}


readInput :: IO (String)
readInput = do
    inputFile <- getDataFileName "input"
    readFile inputFile


main :: IO ()
main = putStrLn "Day {day}"
