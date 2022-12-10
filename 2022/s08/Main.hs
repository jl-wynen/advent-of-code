import Paths_s08
import Data.Array ((!), array, Array)


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

readHeight :: Char -> Int
readHeight h = fromEnum h - fromEnum '0'

data Grid = Grid Int Int (Array Int Int) deriving (Show)

get :: Grid -> Int -> Int -> Int
get (Grid _ ncol heights) irow icol = heights ! (irow * ncol + icol)

parseInput :: [String] -> Grid
parseInput input =
    let nrow = length input
        ncol = length $ head input
        a = [(i, readHeight x) | (i, x) <- zip [0..] $ concat input]
    in Grid nrow ncol $ array (0, nrow*ncol-1) a

countHidden :: Grid -> Int
countHidden grid@(Grid nrow ncol _) =
    length [0 :: Int | irow <- [0..nrow-1]
                     , icol <- [0..ncol-1]
                     , not $ isHidden grid irow icol]

isHidden :: Grid -> Int -> Int -> Bool
isHidden grid@(Grid nrow ncol _) irow icol =
    let height = get grid irow icol
    in any (\i -> (get grid i icol) >= height) [0..irow-1]
       && any (\i -> (get grid i icol) >= height) [irow+1..nrow-1]
       && any (\j -> (get grid irow j) >= height) [0..icol-1]
       && any (\j -> (get grid irow j) >= height) [icol+1..ncol-1]

scenicScores :: Grid -> [Int]
scenicScores grid@(Grid nrow ncol _) =
    [scenicScore grid irow icol | irow <- [0..nrow-1]
                                , icol <- [0..ncol-1]]

-- Like takeWhile but also keep the first element where the predicate is False
takeWhilePrev :: (a -> Bool) -> [a] -> [a]
takeWhilePrev _ [] = []
takeWhilePrev p (h:t) = if p h then h : takeWhilePrev p t else [h]

scenicScore :: Grid -> Int -> Int -> Int
scenicScore grid@(Grid nrow ncol _) irow icol =
    let height = get grid irow icol
        visible = [takeWhilePrev (\i -> (get grid i icol) < height) [irow-1, irow-2 .. 0]
                  ,takeWhilePrev (\i -> (get grid i icol) < height) [irow+1 .. nrow-1]
                  ,takeWhilePrev (\j -> (get grid irow j) < height) [icol-1, icol-2 .. 0]
                  ,takeWhilePrev (\j -> (get grid irow j) < height) [icol+1 .. ncol-1]
                  ]
    in product $ map length visible

main :: IO ()
main = do
    grid <- readInput (parseInput . lines)
    putStrLn $ "Part 1: n visible = " ++ (show $ countHidden grid)
    putStrLn $ "Part 2: max scenic score = " ++ (show $ maximum $ scenicScores grid)
