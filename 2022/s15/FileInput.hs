module FileInput
( parseInput
, readInput
, Pos
) where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec hiding(Pos)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s15

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

type Parser = Parsec Void String
type Pos = (Int, Int)

nothing :: Parser ()
nothing = do return ()

integer :: Parser Int
integer = L.signed nothing L.decimal

symbol :: String -> Parser String
symbol = L.symbol nothing

pos :: Parser Pos
pos = do
    x <- string "x=" *> integer
    _ <- void $ symbol "," *> space1
    y <- string "y=" *> integer
    return (x, y)

sensorAndBeacon :: Parser (Pos, Pos)
sensorAndBeacon = do
    _ <- void $ string "Sensor at "
    sensorPos <- pos
    _ <- void $ string ": closest beacon is at "
    beaconPos <- pos
    return (sensorPos, beaconPos)

listOfsensorAndBeacon :: Parser [(Pos, Pos)]
listOfsensorAndBeacon = sensorAndBeacon `sepBy` (symbol "\n")

parseInput :: IO [(Pos, Pos)]
parseInput = readInput listOfsensorAndBeacon
