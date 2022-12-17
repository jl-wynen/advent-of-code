import           FileInput (parseInput)

import qualified Data.Array.IO as IA
import qualified NDArray       as A

type Grid = A.MArray2D Char

rock :: Char
rock = '#'

air :: Char
air = '.'

sand :: Char
sand = 'o'

applyLines :: [[(Int, Int)]] -> Grid -> IO ()
applyLines [] _ = return ()
applyLines ((h:line):lines') grid = do
    applyLine h line grid
    applyLines lines' grid
applyLines _ _ = error "line with only 1 point"

applyLine :: (Int, Int) -> [(Int, Int)] -> Grid -> IO ()
applyLine _ [] _ = return ()
applyLine a (b:line) grid = do
    fillLine a b grid
    applyLine b line grid

fillLine :: (Int, Int) -> (Int, Int) -> Grid -> IO ()
fillLine (x1, y1) (x2, y2) grid
    | x1 == x2, y1 <= y2 = fillVertical x1 y1 y2 grid
    | x1 == x2, y1 > y2 = fillVertical x1 y2 y1 grid
    | x1 < x2, y1 == y2 = fillHorizontal y1 x1 x2 grid
    | x1 > x2, y1 == y2 = fillHorizontal y1 x2 x1 grid
    | otherwise = error "diagonal line"

fillVertical :: Int -> Int -> Int -> Grid  -> IO ()
fillVertical x y1 y2 grid
    | y1 == y2 = A.mset y1 x rock grid
    | otherwise = do
        A.mset y1 x rock grid
        fillVertical x (y1+1) y2 grid

fillHorizontal :: Int -> Int -> Int -> Grid  -> IO ()
fillHorizontal y x1 x2 grid
    | x1 == x2 = A.mset y x1 rock grid
    | otherwise = do
        A.mset y x1 rock grid
        fillHorizontal y (x1+1) x2 grid

makeGrid :: [[(Int, Int)]] -> IO (Grid, Int)
makeGrid lines' = do
    let maxY = maximum $ map snd $ concat lines'
        nrow = maxY + 2 + 1 -- +1 to include index=maxY
        ncol = 2 * nrow + 1  -- enough space for pyramid of sand
        colOffset = 500 - nrow - 1  -- out source in centre
        offsetLines = map (map (\(x, y) -> (x-colOffset, y))) lines'
    print colOffset
    grid <- A.newArray2D (nrow) (ncol) air
    _ <- applyLines offsetLines grid
    return (grid, colOffset)

moveSand :: (Int, Int) -> Grid -> IO (Maybe (Int, Int))
moveSand (x, y) grid@(A.MArray2D nrow _ _)
    | y==nrow-1 = return Nothing
    | otherwise = do
        below <- A.mget (y+1) x grid
        leftBelow <- A.mget (y+1) (x-1) grid
        rightBelow <- A.mget (y+1) (x+1) grid
        if below == air then moveSand (x, y+1) grid else (
            if leftBelow == air then moveSand (x-1, y+1) grid else (
                if rightBelow == air then moveSand (x+1, y+1) grid else return (Just (x, y))
            ))

fillSand :: (Int, Int) -> Grid -> IO ()
fillSand source grid = do
    pos <- moveSand source grid
    case pos of 
        Just (x, y) -> (A.mset y x sand grid) >> if (x, y) == source
            then return ()
            else fillSand source grid
        Nothing -> return ()

countSand :: Grid -> IO Int
countSand (A.MArray2D _ _ values) = do
    IA.getElems values >>= return . length. filter (==sand)

main :: IO ()
main = do
    (grid, colOffset) <- parseInput >>= makeGrid
    let source = (500-colOffset, 0)
    A.mset (snd source) (fst source) '+' grid

    fillSand source grid
    nSand1 <- countSand grid 
    -- A.drawFreeze grid >>= putStrLn
    putStrLn $ "Part 1: " ++ show nSand1

    fillHorizontal (A.nRow grid - 1) 0 (A.nCol grid - 1) grid
    fillSand source grid
    nSand2 <- countSand grid
    A.drawFreeze grid >>= putStrLn
    putStrLn $ "Part 2: " ++ show nSand2
