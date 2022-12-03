import Paths_s03
import Data.List (intersect)


readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

halve :: [a] -> ([a], [a])
halve l = splitAt n l
    where n = length l `div` 2

priority :: Char -> Int
priority c
    | num >= a && num <= z = num - a + 1
    | num >= aa && num <= zz = num - aa + 27
    | otherwise = 0
    where num = fromEnum c
          a = fromEnum 'a'
          z = fromEnum 'z'
          aa = fromEnum 'A'
          zz = fromEnum 'Z'

intersection :: (String, String) -> Char
intersection (a, b) = head $ a `intersect` b

part1 :: [String] -> IO ()
part1 inputLines = do
    let compartments = map halve inputLines
    putStr "Part 1, sum of priorities = "
    print $ sum $ map (priority . intersection) compartments


triList :: [a] -> (a, a, a)
triList (a:b:c:[]) = (a, b, c)
triList _ = error "bad argument"

groupElves :: [String] -> [(String, String, String)]
groupElves [] = []
groupElves rucksacks = 
    let (h, t) = splitAt 3 rucksacks
    in  triList h : groupElves t

triIntersection :: (String, String, String) -> Char
triIntersection (a, b, c) = head $ a `intersect` b `intersect` c

part2 :: [String] -> IO ()
part2 inputLines = do
    putStr "Part 2, sum of badge priorities = "
    print $ sum $ map (priority . triIntersection) $ groupElves inputLines
    

main :: IO ()
main = do
    input <- readInput lines
    part1 input
    part2 input
