module FileInput
( parseInput
, readInput
, Packet (..)
) where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Paths_s13

readInput :: Parser a -> IO a
readInput parser = do
    filename <- getDataFileName "input"
    content <- readFile filename
    return $ case parse parser filename content of
        Right x -> x
        Left _  -> error "failed to parse input"

data Packet = List [Packet] | Item Int deriving(Eq, Show)

type Parser = Parsec Void String

nothing :: Parser ()
nothing = do return ()

symbol :: String -> Parser String
symbol = L.symbol nothing

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

packet :: Parser Packet
packet = (Item <$> L.decimal)
     <|> (List <$> (brackets $ packet `sepBy` (symbol ",")))

packetPair :: Parser (Packet, Packet)
packetPair = do
    a <- packet
    _ <- void eol
    b <- packet
    return (a, b)

listOfPackets :: Parser [(Packet, Packet)]
listOfPackets = packetPair `sepBy` (symbol "\n\n")

parseInput :: IO [(Packet, Packet)]
parseInput = readInput listOfPackets
