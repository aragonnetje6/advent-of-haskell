module P02 (part1, part2) where

import Text.Parsec (ParseError, Parsec, char, digit, many1, newline, parse, sepBy1)

part1 :: String -> String
part1 = show . length . filter isSafe . unwrap . parse file ""

isSafe :: [Int] -> Bool
isSafe xs = (increasing xs || decreasing xs) && closeEnough xs

increasing :: [Int] -> Bool
increasing (x1 : x2 : xs) = x1 < x2 && increasing (x2 : xs)
increasing _ = True

decreasing :: [Int] -> Bool
decreasing (x1 : x2 : xs) = x1 > x2 && decreasing (x2 : xs)
decreasing _ = True

closeEnough :: [Int] -> Bool
closeEnough (x1 : x2 : xs) = absDiff > 0 && absDiff < 4 && closeEnough (x2 : xs)
  where
    absDiff = abs (x1 - x2)
closeEnough _ = True

unwrap :: Either ParseError a1 -> a1
unwrap (Left e) = error $ show e
unwrap (Right res) = res

integer :: Parsec String () Int
integer = read <$> many1 digit

report :: Parsec String () [Int]
report = sepBy1 integer (char ' ')

file :: Parsec String () [[Int]]
file = many1 (report <* newline)

part2 :: String -> String
part2 = show . length . filter dampenedSafe . unwrap . parse file ""

dampenedSafe :: [Int] -> Bool
dampenedSafe = any isSafe . dampenedOptions

dampenedOptions :: [a0] -> [[a0]]
dampenedOptions xs = map (eliminateIndex xs) [0 .. length xs]

eliminateIndex :: [a0] -> Int -> [a0]
eliminateIndex xs i = take i xs ++ drop (i + 1) xs
