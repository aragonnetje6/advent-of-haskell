module P01 (part1, part2) where
import Text.Parsec (Parsec, many1, digit, parse, spaces, newline)
import Data.Either (fromRight)
import Data.List (sort)

part1 :: String -> String
part1 input = show $ sum $ map (abs . uncurry (-)) (unsplit $ sortBoth $ split $ fromRight [] (parse file "" input))

integer :: Parsec String () Integer
integer = read <$> many1 digit

line :: Parsec String () (Integer, Integer)
line = (,) <$> integer <*> (spaces  *> integer)

file :: Parsec String () [(Integer, Integer)]
file = many1 (line <* newline)

split :: [(a1, a2)] -> ([a1], [a2])
split xs = (map fst xs, map snd xs)

sortBoth :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2])
sortBoth (xs, ys) = (sort xs, sort ys)

unsplit :: ([a1], [a2]) -> [(a1, a2)]
unsplit (xs, ys) = zip xs ys

part2 :: String -> String
part2 input = show $ uncurry similarity $ split $ fromRight [] (parse file "" input)

similarity :: [Integer] -> [Integer] -> Integer
similarity xs ys = sum $ map (\x -> x *  toInteger (length $ filter (== x) ys)) xs
