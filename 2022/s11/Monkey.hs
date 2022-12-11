module Monkey
( Monkey (..)
, Operation
, Target
, Worry
) where
    
type Worry = Int
type Operation = Worry -> Worry
type Target = Worry -> Int
data Monkey = Monkey Int Operation Target
