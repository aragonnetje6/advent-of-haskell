module P01 (part1, part2) where

import Data.Either (fromRight)
import Data.List (sort)
import Text.Parsec (Parsec, digit, many1, newline, parse, spaces)

part1 :: String -> String
part1 = show . sum . map (abs . uncurry (-)) . uncurry zip . sortBoth . unzip . fromRight [] . parse file ""

integer :: Parsec String () Integer
integer = read <$> many1 digit

line :: Parsec String () (Integer, Integer)
line = (,) <$> integer <*> (spaces *> integer)

file :: Parsec String () [(Integer, Integer)]
file = many1 (line <* newline)

sortBoth :: (Ord a1, Ord a2) => ([a1], [a2]) -> ([a1], [a2])
sortBoth (xs, ys) = (sort xs, sort ys)

part2 :: String -> String
part2 = show . uncurry similarity . unzip . fromRight [] . parse file ""

similarity :: [Integer] -> [Integer] -> Integer
similarity xs ys = sum $ map (\x -> x * toInteger (length $ filter (== x) ys)) xs
