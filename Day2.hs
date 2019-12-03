module Day2 where

import Control.Monad   (replicateM)
import Data.Function   ((&))
import Data.List.Index (setAt)
import Data.List.Split (splitOn)

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

getOperation :: Num a => Int -> (a -> a -> a)
getOperation 1 = (+)
getOperation 2 = (*)

compute :: Int -> [Int] -> Int
compute i program
    | code == 99 = head program
    | otherwise  = compute (i + 4) $ setAt position value program
    where
        code = program !! i
        [i1, i2, position] = slice (i + 1) (i + 3) program
        value = getOperation code (program !! i1) (program !! i2)

run :: Int -> Int -> [Int] -> Int
run noun verb = compute 0
              . setAt 1 noun
              . setAt 2 verb

findRun :: Int -> [Int] -> (Int, Int)
findRun expected program = replicateM 2 [0..99]
                         & dropWhile (not . match)
                         & head
                         & format
    where
        match [noun, verb] = run noun verb program == expected
        format [noun, verb] = (noun, verb)

main :: IO ()
main = do
    program <- fmap read . splitOn "," <$> readFile "inputs/day2"

    print $ run 12 2 program 
    print $ findRun 19690720 program
          & \(noun, verb) -> 100 * noun + verb
