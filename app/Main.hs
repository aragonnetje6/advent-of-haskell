module Main where

import qualified P01
import qualified P02
import qualified P03
import qualified P04
import qualified P05
import qualified P06
import qualified P07
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
    Day "./inputs/p03.txt" 3 P03.part1 P03.part2,
    Day "./inputs/p04.txt" 4 P04.part1 P04.part2,
    Day "./inputs/p05.txt" 5 P05.part1 P05.part2,
    Day "./inputs/p06.txt" 6 P06.part1 P06.part2,
    Day "./inputs/p07.txt" 7 P07.part1 P07.part2
  ]

main :: IO ()
main = mapM_ runDay days

runDay :: Day -> IO ()
runDay day =
  readFile
    (inputPath day)
    >>= printAll (dayNumber day) . flip applyAll [part1 day, part2 day]

printAll :: Int -> [String] -> IO ()
printAll day = mapM_ (putStrLn . uncurry (formatAnswer day)) . zip [1 ..]

formatAnswer :: Int -> Int -> String -> String
formatAnswer = printf "Day %d part %d: %s"

applyAll :: String -> [String -> String] -> [String]
applyAll x = map ($ x)
