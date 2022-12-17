module NDArray
( Array2D (..)
, MArray2D (..)
, TwoD, nRow, nCol
, Draw, draw
, drawFreeze
, amap
, freeze
, fromList2D
, get
, mget
, mset
, newArray2D
) where

import           Data.Array    (Array, array, bounds, elems, listArray, (!))
import qualified Data.Array.IO as IA
import           Data.List     (intercalate)

{-
Frozen 2 dimensional array in row-major storage.
-}
data Array2D e =
    Array2D
    Int  -- number of rows
    Int  -- number of columns
    (Array Int e)  -- values
    deriving (Show)

get :: Int -> Int -> Array2D e ->e
get row col (Array2D _ ncol values) = values ! (row * ncol + col)

-- The list is assumed to be row-major, i.e. indexed as (valueList !! row) !! col
fromList2D :: [[e]] -> Array2D e
fromList2D valueList =
    let nrow = length valueList
        ncol = length $ head valueList
        a = zipWith (\i x -> (i, x)) [0..] $ concat valueList
    in Array2D nrow ncol $ array (0, nrow*ncol-1) a

amap :: (e1 -> e2) -> Array2D e1 -> Array2D e2
amap f (Array2D nrow ncol values) = Array2D nrow ncol $ listArray (bounds values) $ map f $ elems values

{-
Mutable 2 dimensional array in row-major storage.
-}
data MArray2D e =
    MArray2D
    Int  -- number of rows
    Int  -- number of columns
    (IA.IOArray Int e)  -- values

newArray2D :: Int -> Int -> e -> IO (MArray2D e)
newArray2D nrow ncol value = do
    values <- IA.newArray (0, nrow*ncol-1) value
    return $ MArray2D nrow ncol values

freeze :: MArray2D e -> IO (Array2D e)
freeze (MArray2D nrow ncol values) = do
    f <- IA.freeze values
    return $ Array2D nrow ncol f

mget :: Int -> Int -> MArray2D e -> IO e
mget row col (MArray2D _ ncol values) =
    IA.readArray values (row * ncol + col)

mset :: Int -> Int -> e -> MArray2D e -> IO ()
mset row col val (MArray2D _ ncol values) =
    IA.writeArray values (row * ncol + col) val

class TwoD a where
    nRow :: a -> Int
    nCol :: a -> Int

instance TwoD (Array2D e) where
    nRow (Array2D n _ _ ) = n
    nCol (Array2D _ n _ ) = n

instance TwoD (MArray2D e) where
    nRow (MArray2D n _ _ ) = n
    nCol (MArray2D _ n _ ) = n


{-
Draw
-}

class Draw a where
    draw :: a -> String

instance Draw Char where
    draw c = [c]

instance Draw Int where
    draw i = show i

instance Draw e => Draw (Array2D e) where
    draw a@(Array2D nrow ncol _) = intercalate "\n" [concat [draw $ get row col a | col <- [0..ncol-1]] | row <- [0..nrow-1]]

drawFreeze :: Draw e => MArray2D e -> IO String
drawFreeze a = freeze a >>= return . draw
