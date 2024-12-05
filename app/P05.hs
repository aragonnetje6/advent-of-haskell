module P05 (part1, part2) where

import Data.Either (fromRight)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import Text.Parsec (Parsec, char, digit, endBy1, many1, newline, parse, sepBy1)

data Rule = Rule Int Int deriving (Show)

part1 :: String -> String
part1 = show . sum . map getMiddle . uncurry filterPages . fromRight ([], []) . parse parseFullFile ""

getMiddle :: [a0] -> a0
getMiddle xs = xs !! (length xs `div` 2)

filterPages :: [Rule] -> [[Int]] -> [[Int]]
filterPages rules = filter (meetsRules rules)

meetsRules :: [Rule] -> [Int] -> Bool
meetsRules rules pages = all (meetsRule pages) rules

meetsRule :: [Int] -> Rule -> Bool
meetsRule pages (Rule first second) = fromMaybe True $ ((<) <$> elemIndex first pages) <*> elemIndex second pages

parseRules :: Parsec String () [Rule]
parseRules = endBy1 parseRule newline

parseRule :: Parsec String () Rule
parseRule = Rule <$> integer <*> (char '|' *> integer)

parsePages :: Parsec String () [Int]
parsePages = sepBy1 integer (char ',')

parseAllPages :: Parsec String () [[Int]]
parseAllPages = endBy1 parsePages newline

parseFullFile :: Parsec String () ([Rule], [[Int]])
parseFullFile = (,) <$> parseRules <*> (newline *> parseAllPages)

integer :: Parsec String () Int
integer = read <$> many1 digit

part2 :: String -> String
part2 = show . sum . map getMiddle . uncurry filterAndFixPages . fromRight ([], []) . parse parseFullFile ""

filterAndFixPages :: [Rule] -> [[Int]] -> [[Int]]
filterAndFixPages rules = map (fix rules) . filter (not . meetsRules rules)

fix :: [Rule] -> [Int] -> [Int]
fix rules = until (meetsRules rules) (`fixRule` rules)

fixRule :: [Int] -> [Rule] -> [Int]
fixRule pages = swapElems pages . fromJust . find (not . meetsRule pages)

swapElems :: [Int] -> Rule -> [Int]
swapElems pages (Rule x y) = take lower pages ++ [pages !! upper] ++ take (upper - lower - 1) (drop (lower + 1) pages) ++ [pages !! lower] ++ drop (upper + 1) pages
  where
    xi = fromJust $ elemIndex x pages
    yi = fromJust $ elemIndex y pages
    lower = min xi yi
    upper = max xi yi
