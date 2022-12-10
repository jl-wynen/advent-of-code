import Prelude hiding (Right, Left)
import Paths_s09
import Data.List (elemIndex, intercalate)
import qualified Data.Set as Set


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

type Move = (Int, Int)

parseMove :: String -> [Move]
parseMove (direction:' ':distance) = replicate (read distance) $ step direction
    where step 'R' = (1, 0)
          step 'L' = (-1, 0)
          step 'U' = (0, 1)
          step 'D' = (0, -1)
          step _ = error "bad direction"
parseMove _ = error "bad input"

parseInput :: String -> [Move]
parseInput = foldr (\line acc -> parseMove line ++ acc) [] . lines

type Pos = (Int, Int)
type Rope = [Pos]

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x1, y1) (x2, y2) = abs (x1-x2) < 2 && abs (y1-y2) < 2

tailPositions :: [Move] -> Rope -> Set.Set Pos
tailPositions [] rope = Set.singleton $ last rope
tailPositions (move:remainder) rope = 
    let newRope = moveRope move rope
    in Set.insert (last newRope) (tailPositions remainder newRope) 

moveRope :: Move -> Rope -> Rope
moveRope _ [] = error "rope must not be empty"
moveRope move (h:t) = updateRope $ moveKnot move h : t

moveKnot :: Move -> Pos -> Pos
moveKnot (mx, my) (px, py) = (mx+px, my+py)

updateRope :: Rope -> Rope
updateRope [] = error "rope must not be empty"
updateRope (t:[]) = [t]
updateRope (h:n:remainder) = h : (updateRope $ updateKnot h n:remainder)

updateKnot :: Pos -> Pos -> Pos
updateKnot h@(hx, hy) t@(tx, ty)
         | isAdjacent h t = t
         | otherwise = (tx + (signum (hx-tx)),
                        ty + (signum (hy-ty)))

drawPositions :: Int -> Int -> Set.Set Pos -> IO ()
drawPositions nx ny positions = do
    putStrLn $ intercalate "\n" [[if Set.member (x, ny-y) positions 
                                  then '#'
                                  else '.'
                                 | x <- [0..nx]]
                                | y <- [0..ny]]

drawRope :: Int -> Int -> Rope -> IO ()
drawRope nx ny rope = do
    putStrLn $ intercalate "\n" [[drawKnot (x, ny-y)
                                 | x <- [0..nx]]
                                | y <- [0..ny]]
    where
        drawKnot knot = case elemIndex knot rope of
            Just 0 -> '#'
            Just i -> drawDigit i
            Nothing -> '.'
        drawDigit d = toEnum (fromEnum '0' + d) :: Char

countVisited :: Int -> [Move] -> Int
countVisited ropeLength moves = 
    Set.size $ tailPositions moves $ replicate ropeLength (0, 0)

main :: IO ()
main = do
    moves <- readInput parseInput
    putStrLn $ "Part 1: n visited = " ++ (show $ countVisited 2 moves)
    putStrLn $ "Part 2: n visited = " ++ (show $ countVisited 10 moves)
