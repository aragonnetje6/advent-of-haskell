module P03 (part1, part2) where

import Data.Either (fromRight)
import Data.Functor (($>))
import Text.Parsec (Parsec, anyChar, char, digit, endBy, eof, lookAhead, many1, manyTill, parse, string, try, (<?>), (<|>))

data Instruction = Mul Int Int | Do | Dont deriving (Show)

part1 :: String -> String
part1 = show . sum . map (uncurry (*)) . fromRight [] . parse muls ""

muls :: Parsec String () [(Int, Int)]
muls = manyTill anyChar (lookAhead $ try mul) *> endBy mul (manyTill anyChar ((eof <?> "end of input") <|> (lookAhead (try mul) $> ())))

mul :: Parsec String () (Int, Int)
mul = (,) <$> (string "mul(" *> integer) <*> (char ',' *> integer) <* char ')'

integer :: Parsec String () Int
integer = read <$> many1 digit

part2 :: String -> String
part2 = show . sum . map (uncurry (*)) . discardDonts . fromRight [] . parse instructions ""

discardDonts :: [Instruction] -> [(Int, Int)]
discardDonts = fst . foldl addIfTrue ([], True)

addIfTrue :: ([(Int, Int)], Bool) -> Instruction -> ([(Int, Int)], Bool)
addIfTrue (xs, _) Do = (xs, True)
addIfTrue (xs, _) Dont = (xs, False)
addIfTrue (xs, True) (Mul x y) = ((x, y) : xs, True)
addIfTrue (xs, False) _ = (xs, False)

instructions :: Parsec String () [Instruction]
instructions = manyTill anyChar (lookAhead $ try instruction) *> endBy instruction (manyTill anyChar ((eof <?> "end of input") <|> (lookAhead (try instruction) $> ())))

instruction :: Parsec String () Instruction
instruction = try (Mul <$> (string "mul(" *> integer) <*> (char ',' *> integer) <* char ')') <|> try (Do <$ string "do()") <|> try (Dont <$ string "don't()")
