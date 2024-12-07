{-# OPTIONS_GHC -Wno-type-defaults #-}

module P07 (part1, part2) where

import GHC.Float (floorFloat)
import Text.Parsec (Parsec, char, digit, endBy1, many1, newline, parse, sepBy1, string)

part1 :: String -> String
part1 = show . sum . map target . filter (isPossible [(+), (*)]) . unwrap . parse parseFile ""

unwrap :: Either a0 a1 -> a1
unwrap (Prelude.Right x) = x
unwrap _ = error "unwrap failed"

data Equation = Equation
  { target :: Integer,
    operands :: [Integer]
  }
  deriving (Show)

type Operator = (Integer -> Integer -> Integer)

parseInteger :: Parsec String () Integer
parseInteger = read <$> many1 digit

parseLine :: Parsec String () Equation
parseLine = Equation <$> parseInteger <*> (string ": " *> sepBy1 parseInteger (char ' '))

parseFile :: Parsec String () [Equation]
parseFile = endBy1 parseLine newline

operantOptions :: [Operator] -> Integer -> [[Operator]]
operantOptions operators 1 = map (: []) operators
operantOptions operators n = concatMap (\op -> map (op :) (operantOptions operators (n - 1))) operators

isPossible :: [Operator] -> Equation -> Bool
isPossible operators eq = any (satisfies eq) $ operantOptions operators $ toInteger (length $ operands eq) - 1

satisfies :: Equation -> [Operator] -> Bool
satisfies eq ops = target eq == total (operands eq) ops
  where
    total (x1 : x2 : xs) (op1 : op1s) = total (op1 x1 x2 : xs) op1s
    total x _ = head x

part2 :: String -> String
part2 = show . sum . map target . filter (isPossible [(+), (*), concatInteger]) . unwrap . parse parseFile ""

concatInteger :: Integer -> Integer -> Integer
concatInteger x y = (y +) $ (x *) $ (10 ^) $ floorFloat $ log (fromInteger y)
