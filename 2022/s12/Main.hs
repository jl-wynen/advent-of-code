import Algorithm.Search (aStar)
import Data.Array (assocs)
import           FileInput (parseInput, Grid, Pos)
import NDArray (Array2D (..), amap, get, draw)


isNeighbor :: Grid -> Pos -> Pos -> Bool
isNeighbor grid@(Array2D nrow ncol _) (row, col) (row2, col2) =
    row2 >= 0 && row2 < nrow &&
    col2 >= 0 && col2 < ncol &&
    (get row2 col2 grid) <= (get row col grid) + 1

neighbors :: Grid -> Pos -> [Pos]
neighbors grid (x, y) = filter (isNeighbor grid (x, y)) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

pathLength :: Pos -> Pos -> Grid -> Maybe Int
pathLength start end grid = case aStar (neighbors grid) distance (distance end) (== end) start of
            Just (len, _) -> Just len
            Nothing -> Nothing

lowestPoints :: Grid -> [Pos]
lowestPoints (Array2D _ ncol values) =
    map (\i -> (i`div`ncol, i`mod`ncol)) $ map fst $ filter (\(_, e) -> e == 0) $ assocs values

dropNothings :: [Maybe e] -> [e]
dropNothings l = [x | Just x <- l]

shortestPathLength :: Pos -> Grid -> Int
shortestPathLength end grid = minimum $ dropNothings $ map (\s -> pathLength s end grid) $ lowestPoints grid

main :: IO ()
main = do
    (start, end, grid) <- parseInput
--     putStrLn $ draw $ amap (\x -> (toEnum (fromEnum 'a' + x))::Char) grid
    let path1 = case pathLength start end grid of
            Just len -> len
            Nothing -> error "failed to find path"
    putStrLn $ "Part 1: " ++ (show path1)
    putStrLn $ "Part 2: " ++ (show $ shortestPathLength end grid)
