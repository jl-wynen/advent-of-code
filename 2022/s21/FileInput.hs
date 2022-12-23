module FileInput
( parseInput
, readInput
, Op (..)
, Monkey (..)
, Operation (..)
) where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s21

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

type Parser = Parsec Void String

data Op = Plus | Minus | Times | Divides | Equals deriving (Show)
data Operation = Input | Num Int | Operation String String Op deriving (Show)
data Monkey = Monkey String Operation deriving (Show)

nothing :: Parser ()
nothing = do return ()

integer :: Parser Int
integer = L.signed nothing L.decimal

op :: Parser Operation
op = do
    a <- many letterChar
    opChar <- space1 *> printChar
    b <- space1 *> many letterChar
    let o = case opChar of
                '+' -> Plus
                '-' -> Minus
                '*' -> Times
                '/' -> Divides
                _ -> error "bad operation"
    return $ Operation a b o

operation :: Parser Operation
operation = (Num <$> integer) <|> op

monkey :: Parser Monkey
monkey = do
    out <- many letterChar
    _ <- void $ char ':' *> space1
    o <- operation
    return $ Monkey out o

parseInput :: IO [Monkey]
parseInput = readInput $ monkey `sepBy` string "\n"
