module FileInput
( parseInput
, Grid
, Pos
) where

import           Paths_s12
import           Data.List (elemIndex, findIndex)
import NDArray (Array2D (..), fromList2D)

type Pos = (Int, Int)
type Grid = Array2D Int

readInput :: (String -> a) -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ parser content

elevation :: Char -> Int
elevation 'E' = fromEnum 'z' - fromEnum 'a'
elevation 'S' = 0
elevation c = fromEnum c - fromEnum 'a'

parseElevation :: [[Char]] -> Grid
parseElevation = fromList2D . map (map elevation)

findPos :: Char -> [[Char]] -> Pos
findPos c charMap =
    let y = expect $ findIndex (\row -> c `elem` row) charMap
        x = expect $ elemIndex c (charMap !! y)
    in (y, x)
    where expect m = case m of
            Just a -> a
            Nothing -> error "did not find element"

parseMap :: [[Char]] -> (Pos, Pos, Grid)
parseMap input = (findPos 'S' input,
                  findPos 'E' input,
                  parseElevation input)

parseInput :: IO (Pos, Pos, Grid)
parseInput = readInput (parseMap . lines)
