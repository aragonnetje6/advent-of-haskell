module P02 (part1, part2) where
import Text.Parsec (Parsec, many1, digit, parse, newline, sepBy1, ParseError, char)

part1 :: String -> String
part1 input = show $ length $ filter isSafe $ unwrap (parse file "" input)

isSafe :: [Integer] -> Bool
isSafe xs = (increasing xs || decreasing xs) && closeEnough xs

increasing :: [Integer] -> Bool
increasing (x1:x2:xs) = x1 < x2 && increasing (x2:xs)
increasing _ = True

decreasing :: [Integer] -> Bool
decreasing (x1:x2:xs) = x1 > x2 && decreasing (x2:xs)
decreasing _ = True

closeEnough :: [Integer] -> Bool
closeEnough (x1:x2:xs) = absDiff > 0 && absDiff < 4 && closeEnough (x2:xs)
  where absDiff = abs (x1 - x2)
closeEnough _ = True

unwrap :: Either ParseError a1 -> a1
unwrap (Left e) = error $ show e
unwrap (Right res) = res

integer :: Parsec String () Integer
integer = read <$> many1 digit

report :: Parsec String () [Integer]
report = sepBy1 integer (char ' ')

file :: Parsec String () [[Integer]]
file = many1 (report <* newline)

part2 :: String -> String
part2 input = show $ length $ filter dampenedSafe $ unwrap (parse file "" input)

dampenedSafe :: [Integer] -> Bool
dampenedSafe xs = any isSafe $ dampenedOptions xs

dampenedOptions :: [a0] -> [[a0]]
dampenedOptions xs = map (eliminateIndex xs) [0..length xs]

eliminateIndex :: [a0] -> Int -> [a0]
eliminateIndex xs i = take i xs ++ drop (i+1) xs
