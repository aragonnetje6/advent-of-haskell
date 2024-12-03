module Main where
import Text.Printf (printf)
import qualified P01
import qualified P02

data DayT = Day String Integer (String -> String) (String -> String)

days :: [DayT]
days = [Day "./inputs/p01.txt" 1 P01.part1 P01.part2,
        Day "./inputs/p02.txt" 2 P02.part1 P02.part2]

main :: IO ()
main = mapM_ runDay days

runDay :: DayT -> IO ()
runDay (Day path day part1 part2) = readFile path >>= printAll day . applyAll [part1, part2]

printAll :: Integer -> [String] -> IO ()
printAll day xs = mapM_ (putStrLn . formatAnswer day) $ zip [1..] xs

formatAnswer :: Integer -> (Integer, String) -> String
formatAnswer day (part, answer) = printf "Day %d part %d: %s" day part answer

applyAll :: [String -> String] -> String -> [String]
applyAll fs x = map (\f -> f x) fs
