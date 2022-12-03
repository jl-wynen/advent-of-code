import Paths_s{day}


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f


main :: IO ()
main = do
    input <- readInput id
    print input
