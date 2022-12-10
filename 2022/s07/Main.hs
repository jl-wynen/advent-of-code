import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Paths_s07


maxDirSize :: Int
maxDirSize = 100000

totalDiskSpace :: Int
totalDiskSpace = 70000000

requiredDiskSpace :: Int
requiredDiskSpace = 30000000

readInput :: (String -> a) -> IO a
readInput f = do
    getDataFileName "input" >>= readFile >>= return . f

data Line =
    Cd String |
    Ls |
    File Int |
    Dir String
    deriving (Show)

parseLine :: String -> Line
parseLine str = case (words str) of
    ["$","cd",dir] -> Cd dir
    ["$", "ls"]    -> Ls
    ["dir", dir]   -> Dir dir
    [size, _]      -> File $ read size
    _              -> error "bad line"

pathFromStack :: [String] -> String
pathFromStack = intercalate "/" . reverse

insertSizeAt :: Map.Map String Int -> [String] -> Int -> Map.Map String Int
insertSizeAt sizes dir size = snd $ Map.insertLookupWithKey (\_ old new -> old+new) (pathFromStack dir) size sizes

insertSize :: Map.Map String Int -> [String] -> Int -> Map.Map String Int
insertSize sizes [] size = insertSizeAt sizes [] size
insertSize sizes dir@(_:parent) size =
    let newSizes = insertSizeAt sizes dir size
    in insertSize newSizes parent size

listSizes :: [String] -> Map.Map String Int -> [Line] -> Map.Map String Int
listSizes _ sizes [] = sizes

listSizes currentDir sizes (Cd dir : remainder) =
    let nextDir = case dir of
            "/"  -> []
            ".." -> tail currentDir
            _    -> dir : currentDir
    in listSizes nextDir sizes remainder

listSizes dir sizes (File size : remainder) =
    let newSizes = insertSize sizes dir size
    in listSizes dir newSizes remainder

listSizes dir sizes (Ls : remainder) =
    listSizes dir sizes remainder

listSizes dir sizes (Dir _ : remainder) =
    listSizes dir sizes remainder

main :: IO ()
main = do
    input <- readInput (map parseLine . lines)
    let sizes = listSizes [] Map.empty input
    let totalUsedSize = sizes Map.! ""
    putStr "Part 1: "
    print $ Map.foldr (+) 0 $ Map.filter (< maxDirSize) sizes
    putStr "Part 2: "
    print $ minimum
          $ filter (\size -> totalDiskSpace - (totalUsedSize - size) >= requiredDiskSpace)
          $ Map.elems sizes
