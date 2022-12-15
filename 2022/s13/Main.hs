import           FileInput (parseInput, Packet (..))

packetOrder :: (Packet, Packet) -> Ordering
packetOrder (Item a, Item b) = a `compare` b
packetOrder (List [], _) = LT
packetOrder (Item a, List b) = packetOrder (List [Item a], List b)
packetOrder (List a, Item b) = packetOrder (List a, List [Item b])
packetOrder (List (a:as), List (b:bs)) = case packetOrder (a, b) of
    LT -> LT
    GT -> GT
    EQ -> packetOrder (List as, List bs)
packetOrder _ = GT

correctOrder :: (Packet, Packet) -> Bool
correctOrder x = packetOrder x /= GT

main :: IO ()
main = do
    packetPairs <- parseInput
    putStrLn $ "Part 1: " ++ (show 
        $ sum
        $ map fst
        $ filter snd
        $ zip ([1..] :: [Int]) (map correctOrder packetPairs))
    print $ map correctOrder packetPairs
