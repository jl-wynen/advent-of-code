module Monkey
( Monkey (..)
, Operation
, Target
) where
    
type Operation = Int -> Int
type Target = Int -> Int
data Monkey = Monkey Int Operation Target
