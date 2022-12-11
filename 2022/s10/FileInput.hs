module FileInput (parseInput, Instruction (..)) where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s10

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

type Parser = Parsec Void String

data Instruction = Noop | Addx Int deriving (Show, Read)

nothing :: Parser ()
nothing = do return ()

integer :: Parser Int
integer = L.signed nothing L.decimal

eoi :: Parser ()
eoi = (void eol) <|> eof

instruction :: Parser Instruction
instruction = (
        Noop <$ string "noop"
    <|> Addx <$> (string "addx" *> space1 *> integer)
    ) <* eoi

instructions :: Parser [Instruction]
instructions = many instruction

parseInput :: IO [Instruction]
parseInput = do
    readInput instructions
