import           FileInput (parseInput, Pos)
import Data.List (nub, sortBy, sortOn)

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs(x1-x2) + abs(y1-y2)

distance' :: (Pos, Pos) -> Int
distance' = uncurry distance

coverageXBounds :: [(Pos, Pos)] -> (Int, Int)
coverageXBounds = foldr f (maxBound :: Int, minBound :: Int)
    where f (s@(sx, _), b) (mi, ma) =
            let d = distance s b
            in (min mi (sx-d), max ma (sx+d))

inRangeY :: Int -> [(Pos, Pos)] -> [(Pos, Pos)]
inRangeY _ [] = []
inRangeY y ((sensor@(_, sy), beacon):remainder) =
    if distance sensor beacon >= abs(y-sy)
        then (sensor, beacon):inRangeY y remainder
        else inRangeY y remainder

mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals intervals =
    f (head sorted) (tail sorted)
    where sorted = sortOn fst intervals
          f interval [] = [interval]
          f (b1, e1) ((b2, e2):t) =
            if e1 < b2
            then (b1, e1) : f (b2, e2) t
            else f (b1, max e1 e2) t

coveredIntervals :: Int -> [(Pos, Pos)] -> [(Int, Int)]
coveredIntervals y sensorsAndBeacons =
    mergeIntervals $ map xRange $ inRangeY y sensorsAndBeacons
    where xRange (s@(sx, sy), b) =
            let dx = distance s b - abs(sy-y)
            in (sx-dx, sx+dx)

notCoveredAtY :: Int -> (Int, Int) -> [(Pos, Pos)] -> [Pos]
notCoveredAtY y (minX, maxX) sensorsAndBeacons =
    f minX $ coveredIntervals y $ inRangeY y sensorsAndBeacons
    where f x []
            | x > maxX = []
            | otherwise = (x, y) : f (x+1) []
          f x ((mi, ma):intervals)
            | x > maxX = []
            | x < mi = (x, y) : f (x+1) ((mi, ma):intervals)
            | otherwise = f (ma+1) intervals


main :: IO ()
main = do
    input <- parseInput
    let sensorsAndBeacons = sortBy (\a b -> distance' b `compare` distance' a) input
        (minX, maxX) = coverageXBounds sensorsAndBeacons
        refY = 2000000
        nCovered = (maxX-minX+1) - (length $ notCoveredAtY refY (minX, maxX) sensorsAndBeacons)
        beacons = map snd sensorsAndBeacons
        nBeaconsAtY = length $ nub $ map snd $ filter (\(_, by) -> by == refY) beacons
    putStrLn $ "Part 1: " ++ (show $ nCovered - nBeaconsAtY)

    let maxCoord = 4000000
        notCovered = head 
                   $ head 
                   $ filter (not . null) 
                   $ map (\y -> notCoveredAtY y (0, maxCoord) sensorsAndBeacons) [0..maxCoord]
    putStrLn $ "Part 2: pos=" ++ (show notCovered) ++ "  frequency=" ++ 
                (show $ fst notCovered * 4000000 + snd notCovered)
