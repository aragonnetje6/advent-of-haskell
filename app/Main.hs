module Main where

import qualified P01
import qualified P02
import qualified P03
import qualified P04
import qualified P05
import Text.Printf (printf)

data Day = Day
  { inputPath :: String,
    dayNumber :: Int,
    part1 :: String -> String,
    part2 :: String -> String
  }

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
runDay day = readFile (inputPath day) >>= printAll (dayNumber day) . flip applyAll [part1 day, part2 day]

printAll :: Int -> [String] -> IO ()
printAll day = mapM_ (putStrLn . uncurry (formatAnswer day)) . zip [1 ..]

formatAnswer :: Int -> Int -> String -> String
formatAnswer = printf "Day %d part %d: %s"

applyAll :: String -> [String -> String] -> [String]
applyAll x = map ($ x)
