import Data.List (sortBy)

import FileInput (parseInput)
import Monkey (Monkey (..))

throwItem :: Monkey -> Int -> (Int, Int)
throwItem (Monkey _ op target) worry =
    let newWorry = (op worry) `div` 3
    in (target newWorry, newWorry)

throwItems :: Monkey -> Int -> [Int] -> [[Int]]
throwItems monkey nMonkeys worries =
    let targets = map (throwItem monkey) worries
    in [[w | (t, w) <- targets, t == i] | i <- [0..nMonkeys-1]]

processMonkey :: Monkey -> [[Int]] -> (Int, [[Int]])
processMonkey monkey@(Monkey imonkey _ _) worries =
    let currentItems = worries !! imonkey
        thrown = throwItems monkey (length worries) currentItems
        newWorries = [if i == imonkey then [] else w ++ t
                     | (i, w, t) <- zip3 [0..] worries thrown]
    in (length currentItems, newWorries)

doRound :: [Monkey] -> [Int] -> [[Int]] -> ([Int], [[Int]])
doRound [] [] worries = ([], worries)
doRound (monkey:ms) (nInspected:ns) worries = 
    let (n, newWorries) = processMonkey monkey worries
        (newNInspected, resultWorries) = doRound ms ns newWorries
    in ((nInspected+n):newNInspected, resultWorries)
doRound _ _ _ = error "length of inspect counts and monkeys must be the same"

playKeepAway :: [Monkey] -> [[Int]] -> ([Int], [[Int]])
playKeepAway monkeys worries =
    iterate (\(n, w) -> doRound monkeys n w) ((replicate (length monkeys) 0), worries) !! 20

main :: IO ()
main = do
    (startingWorry, monkeys) <- parseInput
    let nInspected = fst $ playKeepAway monkeys startingWorry
    putStrLn $ "Part 1: " ++ (show
                             $ product
                             $ take 2
                             $ sortBy (\a b -> b `compare` a) nInspected)
