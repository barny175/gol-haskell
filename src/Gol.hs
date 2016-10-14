module Gol (
    Cell(..)
    ,generateRandomList
)
where

import System.Random

data Cell = Live | Dead deriving (Show, Enum, Bounded, Eq)

instance Random Cell where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

generateRandomList :: (RandomGen g) => Float -> g -> [Cell]
generateRandomList percent gen = map toCell $ (randomRs (0, 1) gen :: [Float])
    where toCell x = if x < percent then Live else Dead
