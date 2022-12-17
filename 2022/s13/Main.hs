import           FileInput (parseInput, Packet (..))
import Data.List (findIndices, sortBy)

packetOrder :: Packet -> Packet -> Ordering
packetOrder (Item a) (Item b)
    | a < b = LT
    | a > b = GT
    | otherwise = EQ
packetOrder (Item a) (List b) = packetOrder (List [Item a]) (List b)
packetOrder (List a) (Item b) = packetOrder (List a) (List [Item b])
packetOrder (List []) (List []) = EQ
packetOrder (List []) _ = LT
packetOrder _(List []) = GT
packetOrder (List (a:as)) (List (b:bs)) = case packetOrder a b of
    EQ -> packetOrder (List as) (List bs)
    x -> x

correctOrder :: (Packet, Packet) -> Bool
correctOrder (a, b) = packetOrder a b /= GT

flatten :: [(Packet, Packet)] -> [Packet]
flatten [] = []
flatten ((a, b):remainder) = [a, b] ++ flatten remainder

main :: IO ()
main = do
    packetPairs <- parseInput
    putStrLn $ "Part 1: " ++ (show 
        $ sum
        $ map fst
        $ filter snd
        $ zip ([1..] :: [Int]) (map correctOrder packetPairs))
    
    let dividers = [List [List [Item 2]], List [List [Item 6]]]
    putStrLn $ "Part 2: " ++ (show
        $ product
        $ map (+1)
        $ findIndices (`elem` dividers)
        $ sortBy packetOrder
        $ dividers ++ (flatten packetPairs))
