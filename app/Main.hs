module Main where

import qualified P01
import qualified P02
import qualified P03
import qualified P04
import qualified P05
import Text.Printf (printf)

data Day = Day String Int (String -> String) (String -> String)

days :: [Day]
days =
  [ Day "./inputs/p01.txt" 1 P01.part1 P01.part2,
    Day "./inputs/p02.txt" 2 P02.part1 P02.part2,
    Day "./inputs/p03.txt" 2 P03.part1 P03.part2,
    Day "./inputs/p04.txt" 2 P04.part1 P04.part2,
    Day "./inputs/p05.txt" 2 P05.part1 P05.part2
  ]

main :: IO ()
main = mapM_ runDay days

runDay :: Day -> IO ()
runDay (Day path day part1 part2) = readFile path >>= printAll day . applyAll [part1, part2]

printAll :: Int -> [String] -> IO ()
printAll day xs = mapM_ (putStrLn . formatAnswer day) $ zip [1 ..] xs

formatAnswer :: Int -> (Int, String) -> String
formatAnswer day (part, answer) = printf "Day %d part %d: %s" day part answer

applyAll :: [String -> String] -> String -> [String]
applyAll fs x = map (\f -> f x) fs
