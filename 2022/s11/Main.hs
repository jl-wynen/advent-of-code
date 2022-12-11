import Data.List (sortBy, union)

import FileInput (parseInput)
import Monkey (Monkey (..), Worry)

isPrime :: Int -> Bool
isPrime i = all (/=0) [i `mod` x | x <- [2..(i`div`2)]]

primeFactors :: Int -> [Int]
primeFactors i 
    | isPrime i = [i]
    | otherwise = [f | f <- [1..(i`div`2)], isPrime f, i`mod`f == 0]

throwItem :: Monkey -> Worry -> (Int, Worry)
throwItem (Monkey _ op target) worry =
    let newWorry = op worry
    in (target newWorry, newWorry)

throwItems :: Monkey -> Int -> [Worry] -> [[Worry]]
throwItems monkey nMonkeys worries =
    let targets = map (throwItem monkey) worries
    in [[w | (t, w) <- targets, t == i] | i <- [0..nMonkeys-1]]

processMonkey :: Monkey -> [[Worry]] -> (Int, [[Worry]])
processMonkey monkey@(Monkey imonkey _ _) worries =
    let currentItems = worries !! imonkey
        thrown = throwItems monkey (length worries) currentItems
        newWorries = [if i == imonkey then [] else w ++ t
                     | (i, w, t) <- zip3 [0..] worries thrown]
    in (length currentItems, newWorries)

doRound :: [Monkey] -> [Int] -> [[Worry]] -> ([Int], [[Worry]])
doRound [] [] worries = ([], worries)
doRound (monkey:ms) (nInspected:ns) worries = 
    let (n, newWorries) = processMonkey monkey worries
        (newNInspected, resultWorries) = doRound ms ns newWorries
    in ((nInspected+n):newNInspected, resultWorries)
doRound _ _ _ = error "length of inspect counts and monkeys must be the same"

playKeepAway :: Int -> [Monkey] -> [[Worry]] -> ([Int], [[Worry]])
playKeepAway nTurns monkeys worries =
    let initial = ((replicate (length monkeys) 0), worries)
    in head $ drop nTurns $ iterate (\(n, w) -> doRound monkeys n w) initial

business :: [Int] -> Int
business = product
         . take 2
         . sortBy (\a b -> b `compare` a)

main :: IO ()
main = do
    (startingWorry, divisors, monkeys) <- parseInput
    let primes = foldr union [] $ map primeFactors divisors
        modulus = product primes
        part1Monkeys = map (\(Monkey i op t) -> Monkey i ((`div` 3) . op) t) monkeys
        part2Monkeys = map (\(Monkey i op t) -> Monkey i ((`mod` modulus) . op) t) monkeys
        nInspected1 = fst $ playKeepAway 20 part1Monkeys startingWorry
        nInspected2 = fst $ playKeepAway 10000 part2Monkeys startingWorry
    putStrLn $ "Part 1: " ++ (show $ business nInspected1)
    putStrLn $ "Part 2: " ++ (show $ business nInspected2)
