import Paths_s{day}


readInput :: IO (String)
readInput = do
    inputFile <- getDataFileName "input"
    readFile inputFile


main :: IO ()
main = do
    input <- parseInput
    print input
