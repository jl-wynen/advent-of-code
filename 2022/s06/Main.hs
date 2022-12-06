import           Data.List (find, nub)
import           Paths_s06


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

findMarker :: Int -> String -> Int
findMarker sequenceLength stream =
    let res = find ((==sequenceLength) . length . nub . snd)
            $ zip [0..]
            $ (uncurry $ scanl (\acc x -> (tail acc)++[x]))
            $ splitAt sequenceLength stream
    in case res of
        Just (i, _) -> i+sequenceLength
        _           -> error "bad input"

findStartOfPacket :: String -> Int
findStartOfPacket = findMarker 4

findStartOfMessage :: String -> Int
findStartOfMessage = findMarker 14

main :: IO ()
main = do
    input <- readInput id
    putStrLn $ "Part 1: " ++ (show $ findStartOfPacket input)
    putStrLn $ "Part 2: " ++ (show $ findStartOfMessage input)
