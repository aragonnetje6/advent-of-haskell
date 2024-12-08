module P04 (part1, part2) where

import Data.List (transpose)

part1 :: String -> String
part1 = show . countXmas . lines

countXmas :: [String] -> Int
countXmas text = countHorizontal text + countVertical text + countDiagonal text

countHorizontal :: [String] -> Int
countHorizontal = sum . map countString

countVertical :: [String] -> Int
countVertical = countHorizontal . transpose

countDiagonal :: [String] -> Int
countDiagonal =
  sum
    . map countString
    . concatMap getDiagonals
    . concatMap transpose
    . windows 4
    . map (windows 4)

getDiagonals :: [String] -> [String]
getDiagonals strings =
  [ zipWith
      (flip (!!))
      [0 ..]
      strings,
    zipWith (\i xs -> xs !! (length (head strings) - i)) [1 ..] strings
  ]

countString :: String -> Int
countString = length . filter (\x -> x == "XMAS" || reverse x == "XMAS") . windows 4

windows :: Int -> [a0] -> [[a0]]
windows n xs = map (\i -> take n $ drop i xs) [0 .. length xs - n]

part2 :: String -> String
part2 = show . masx . lines

masx :: [String] -> Int
masx =
  length
    . filter isMasx
    . map getDiagonals
    . concatMap transpose
    . windows 3
    . map (windows 3)

isMasx :: [String] -> Bool
isMasx (x : y : _) = (x == "MAS" || x == "SAM") && (y == "MAS" || y == "SAM")
isMasx _ = error "wrong pair"
