module P03 (part1, part2) where

import Data.Either (fromRight)
import Data.Functor (($>))
import Text.Parsec (Parsec, anyChar, char, digit, endBy, eof, lookAhead, many1, manyTill, parse, string, try, (<?>), (<|>))

data Instruction = Mul Integer Integer | Do | Dont

instance Show Instruction where
  show (Mul x y) = show (x, y)
  show Do = "Do"
  show Dont = "Dont"

part1 :: String -> String
part1 = show . sum . map (uncurry (*)) . fromRight [] . parse muls ""

muls :: Parsec String () [(Integer, Integer)]
muls = manyTill anyChar (lookAhead $ try mul) *> endBy mul (manyTill anyChar ((eof <?> "end of input") <|> (lookAhead (try mul) $> ())))

mul :: Parsec String () (Integer, Integer)
mul = (,) <$> (string "mul(" *> integer) <*> (char ',' *> integer) <* char ')'

integer :: Parsec String () Integer
integer = read <$> many1 digit

part2 :: String -> String
part2 = show . sum . map (uncurry (*)) . discardDonts . fromRight [] . parse instructions ""

discardDonts :: [Instruction] -> [(Integer, Integer)]
discardDonts = fst . foldl addIfTrue ([], True)

addIfTrue :: ([(Integer, Integer)], Bool) -> Instruction -> ([(Integer, Integer)], Bool)
addIfTrue (xs, _) Do = (xs, True)
addIfTrue (xs, _) Dont = (xs, False)
addIfTrue (xs, True) (Mul x y) = ((x, y) : xs, True)
addIfTrue (xs, False) _ = (xs, False)

instructions :: Parsec String () [Instruction]
instructions = manyTill anyChar (lookAhead $ try instruction) *> endBy instruction (manyTill anyChar ((eof <?> "end of input") <|> (lookAhead (try instruction) $> ())))

instruction :: Parsec String () Instruction
instruction = try (Mul <$> (string "mul(" *> integer) <*> (char ',' *> integer) <* char ')') <|> try (Do <$ string "do()") <|> try (Dont <$ string "don't()")
