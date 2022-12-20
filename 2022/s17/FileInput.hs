module FileInput ( readInput ) where

import           Paths_s17

readInput :: IO String
readInput = do
    filename <- getDataFileName "input"
    readFile filename
