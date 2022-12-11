module FileInput
( parseInput
, readInput
) where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s{day}

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

type Parser = Parsec Void String

parseInput :: IO InputType
parseInput = readInput parser
