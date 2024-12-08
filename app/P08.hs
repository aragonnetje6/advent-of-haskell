{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module P08 (part1, part2) where

import Data.Function (on)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (catMaybes)
import Text.Parsec
  ( ParseError,
    Parsec,
    char,
    endBy,
    getPosition,
    many1,
    newline,
    noneOf,
    parse,
    sourceColumn,
    sourceLine,
    (<|>),
  )

part1 :: String -> String
part1 input =
  show
    $ length
    $ nub
    $ concatMap
      ( filter
          ( antinodeInBounds
              (minimum xs)
              (minimum ys)
              (maximum xs)
              (maximum ys)
          )
          . getAntinodes
      )
    $ groupFrequencies antennae
  where
    antennae = unwrap $ parse parseFile "" input
    xs = map antennaX antennae
    ys = map antennaY antennae

unwrap :: Either ParseError a1 -> a1
unwrap (Left e) = error $ show e
unwrap (Right res) = res

data Antenna = Antenna
  { frequency :: Char,
    antennaX :: Int,
    antennaY :: Int
  }
  deriving (Show)

data Antinode = AN
  { anX :: Int,
    anY :: Int
  }
  deriving (Show, Eq, Ord)

sameFrequency :: Antenna -> Antenna -> Bool
sameFrequency a b = frequency a == frequency b

groupFrequencies :: [Antenna] -> [[Antenna]]
groupFrequencies = groupBy sameFrequency . sortBy (compare `on` frequency)

getAntinodes :: [Antenna] -> [Antinode]
getAntinodes (a : as) =
  concatMap
    (antinodes a)
    as
    ++ getAntinodes as
getAntinodes _ = []

antinodeInBounds :: Int -> Int -> Int -> Int -> Antinode -> Bool
antinodeInBounds minX minY maxX maxY (AN x y) = x >= minX && x <= maxX && y >= minY && y <= maxY

antinodes :: Antenna -> Antenna -> [Antinode]
antinodes (Antenna _ x1 y1) (Antenna _ x2 y2) =
  [ AN (x1 + x1 - x2) (y1 + y1 - y2),
    AN (x2 + x2 - x1) (y2 + y2 - y1)
  ]

parseAntenna :: Parsec String () Antenna
parseAntenna =
  (\c pos -> Antenna c (sourceColumn pos) (sourceLine pos))
    <$> noneOf ".\n"
    <*> getPosition

parseTile :: Parsec String () (Maybe Antenna)
parseTile = Nothing <$ char '.' <|> Just <$> parseAntenna

parseFile :: Parsec String () [Antenna]
parseFile = concatMap catMaybes <$> endBy (many1 parseTile) newline

part2 :: String -> String
part2 input =
  show
    $ length
    $ nub
    $ concatMap
      ( getLines
          (antinodeInBounds minX minY maxX maxY)
      )
    $ groupFrequencies antennae
  where
    antennae = unwrap $ parse parseFile "" input
    xs = map antennaX antennae
    ys = map antennaY antennae
    minX = minimum xs
    minY = minimum ys
    maxX = maximum xs
    maxY = maximum ys

getLines :: (Antinode -> Bool) -> [Antenna] -> [Antinode]
getLines bounds (a : as) =
  concatMap (line bounds a) as
    ++ getLines bounds as
getLines _ [] = []

line :: (Antinode -> Bool) -> Antenna -> Antenna -> [Antinode]
line bounds a1 a2 =
  extend bounds a1 x y
    ++ extend bounds a1 (-x) (-y)
  where
    diffX = antennaX a2 - antennaX a1
    diffY = antennaY a2 - antennaY a1
    gcdDiff = gcd diffX diffY
    x = diffX `div` gcdDiff
    y = diffY `div` gcdDiff

extend :: (Antinode -> Bool) -> Antenna -> Int -> Int -> [Antinode]
extend bounds (Antenna _ baseX baseY) x y =
  tail $
    until
      (not . bounds . head)
      (\(a@(AN ax ay) : as) -> AN (ax + x) (ay + y) : a : as)
      [AN baseX baseY]
