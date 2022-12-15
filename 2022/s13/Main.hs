import           FileInput (parseInput, Packet (..))

import Debug.Trace (trace)

data Result = InOrder | OutOfOrder | KeepGoing deriving (Eq)

packetOrder :: (Packet, Packet) -> Result
packetOrder (a,b) | trace ("packetOrder " ++ show a ++ "\n            " ++ show b) False = undefined
packetOrder (Item a, Item b)
    | a < b = InOrder
    | a > b = OutOfOrder
    | otherwise = KeepGoing
packetOrder (Item a, List b) = packetOrder (List [Item a], List b)
packetOrder (List a, Item b) = packetOrder (List a, List [Item b])
packetOrder (List [], List []) = KeepGoing
packetOrder (List [], _) = InOrder
packetOrder (_, List []) = OutOfOrder
packetOrder (List (a:as), List (b:bs)) = case packetOrder (a, b) of
    KeepGoing -> packetOrder (List as, List bs)
    x -> x

correctOrder :: (Packet, Packet) -> Bool
correctOrder x = packetOrder x /= OutOfOrder


main :: IO ()
main = do
    packetPairs <- parseInput
    putStrLn $ "Part 1: " ++ (show 
        $ sum
        $ map fst
        $ filter snd
        $ zip ([1..] :: [Int]) (map correctOrder packetPairs))
    print $ map correctOrder packetPairs
