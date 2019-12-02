module Day1 where

import Data.Function ((&))

requiredFuel :: Int -> Int
requiredFuel mass = mass `div` 3 - 2

totalRequiredFuel :: Int -> Int
totalRequiredFuel mass = requiredFuel `iterate` mass
                       & takeWhile (> 0)
                       & tail
                       & sum

main :: IO ()
main = do
    modules <- fmap read . lines <$> readFile "inputs/day1"

    print . sum $ requiredFuel <$> modules
    print . sum $ totalRequiredFuel <$> modules
