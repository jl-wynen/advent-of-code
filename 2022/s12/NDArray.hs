module NDArray
( Array2D (..)
, Draw, draw
, amap
, fromList2D
, get
) where

import Data.Array ((!), array, bounds, elems, listArray, Array)
import Data.List (intercalate)

{-
2 dimensional array in row-major storage.
-}
data Array2D e =
    Array2D
    Int  -- number of rows
    Int  -- number of columns
    (Array Int e)  -- values
    deriving (Show)

get :: Int -> Int ->  Array2D e ->e
get row col (Array2D _ ncol values) = values ! (row * ncol + col)

-- The list is assumed to be row-major, i.e. indexed as (valueList !! row) !! col
fromList2D :: [[e]] -> Array2D e
fromList2D valueList =
    let nrow = length valueList
        ncol = length $ head valueList
        a = zipWith (\i x -> (i, x)) [0..] $ concat valueList
    in Array2D nrow ncol $ array (0, nrow*ncol-1) a

class Draw a where
    draw :: a -> String

instance Draw Char where
    draw c = [c]

instance Draw Int where
    draw i = show i

instance Draw e => Draw (Array2D e) where
    draw a@(Array2D nrow ncol _) = intercalate "\n" [concat [draw $ get row col a | col <- [0..ncol-1]] | row <- [0..nrow-1]]

amap :: (e1 -> e2) -> Array2D e1 -> Array2D e2
amap f (Array2D nrow ncol values) = Array2D nrow ncol $ listArray (bounds values) $ map f $ elems values
