module Main where

import qualified P01
import qualified P02
import qualified P03
import qualified P04
import Text.Printf (printf)

data DayT = Day String Int (String -> String) (String -> String)

days :: [DayT]
days =
  [ Day "./inputs/p01.txt" 1 P01.part1 P01.part2,
    Day "./inputs/p02.txt" 2 P02.part1 P02.part2,
    Day "./inputs/p03.txt" 2 P03.part1 P03.part2,
    Day "./inputs/p04.txt" 2 P04.part1 P04.part2
  ]

main :: IO ()
main = mapM_ runDay days

runDay :: DayT -> IO ()
runDay (Day path day part1 part2) = readFile path >>= printAll day . applyAll [part1, part2]

printAll :: Int -> [String] -> IO ()
printAll day xs = mapM_ (putStrLn . formatAnswer day) $ zip [1 ..] xs

formatAnswer :: Int -> (Int, String) -> String
formatAnswer day (part, answer) = printf "Day %d part %d: %s" day part answer

applyAll :: [String -> String] -> String -> [String]
applyAll fs x = map (\f -> f x) fs
