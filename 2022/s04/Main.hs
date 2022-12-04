import           Paths_s04


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

splitOn :: Char -> String -> [String]
splitOn sep str = words [if c == sep then ' ' else c | c <- str]

data Range = Range { begin :: Int
                   , end   :: Int
                   } deriving (Show)

parseRange :: String -> Range
parseRange = toRange . splitOn '-'
    where toRange (b:e:_) = Range (read b) (read e)
          toRange _       = error "bad argument"

parse :: String -> [(Range, Range)]
parse = map (rangePair . splitOn ',') . lines
    where rangePair (a:b:_) = (parseRange a, parseRange b)
          rangePair _       = error "bad argument"

oneContainedInOther :: (Range, Range) -> Bool
oneContainedInOther (a, b) = b1 >= b2 && e1 <= e2    -- a in b
                            || b1 <= b2 && e1 >= e2  -- b in a
                           where b1 = begin a
                                 b2 = begin b
                                 e1 = end a
                                 e2 = end b

overlaps :: (Range, Range) -> Bool
overlaps (a, b) = b1 <= b2 && e1 >= b2
               || b1 >= b2 && b1 <= e2
               where b1 = begin a
                     b2 = begin b
                     e1 = end a
                     e2 = end b


main :: IO ()
main = do
    input <- readInput parse
    putStr "Part 1: "
    print $ length $ filter oneContainedInOther input
    putStr "Part 2: "
    print $ length $ filter overlaps input
