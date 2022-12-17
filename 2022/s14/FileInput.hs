module FileInput
( parseInput
, readInput
, Line
) where

import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s14

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

type Parser = Parsec Void String
type Line = [(Int, Int)]

nothing :: Parser ()
nothing = do return ()

symbol :: String -> Parser String
symbol = L.symbol nothing

point :: Parser (Int, Int)
point = do
    a <- L.decimal
    _ <- symbol ","
    b <- L.decimal
    return (a, b)

line :: Parser Line
line = point `sepBy` (symbol " -> ")

lines' :: Parser [Line]
lines' = line `sepBy` (symbol "\n")

parseInput :: IO [Line]
parseInput = readInput lines'
