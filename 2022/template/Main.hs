import           FileInput (parseInput)

main :: IO ()
main = do
    input <- parseInput
    print input
