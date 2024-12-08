{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module P06 (part1, part2) where

import Data.Either (lefts, rights)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Text.Parsec
  ( Parsec,
    char,
    endBy1,
    getPosition,
    many1,
    newline,
    parse,
    sourceColumn,
    sourceLine,
    (<|>),
  )

part1 :: String -> String
part1 input = show $ length $ nub $ uncurry (finalWalk walls) $ walkUntilGone walls guard
  where
    (walls, guard) = unwrap $ parse grid "" input

unwrap :: Either a0 a1 -> a1
unwrap (Prelude.Right x) = x
unwrap _ = error "unwrap failed"

data Guard = Guard
  { guardX :: Int,
    guardY :: Int,
    guardDir :: Direction
  }
  deriving (Eq)

data Wall = Wall
  { wallX :: Int,
    wallY :: Int
  }
  deriving (Eq)

data Direction = Up | Down | DLeft | DRight deriving (Show, Eq)

tile :: Parsec String () (Maybe (Either Wall Guard))
tile =
  (Nothing <$ char '.')
    <|> ( ( \pos ->
              Just (Prelude.Left (Wall (sourceColumn pos) (sourceLine pos)))
          )
            <$> getPosition
            <* char '#'
        )
    <|> ( ( \pos ->
              Just (Prelude.Right (Guard (sourceColumn pos) (sourceLine pos) Down))
          )
            <$> getPosition
            <* char '^'
        )

grid :: Parsec String () ([Wall], Guard)
grid = getGuard . concatMap catMaybes <$> endBy1 (many1 tile) newline

getGuard :: [Either Wall Guard] -> ([Wall], Guard)
getGuard eithers = (lefts eithers, head $ rights eithers)

walkUntilGone :: [Wall] -> Guard -> (Guard, [(Int, Int)])
walkUntilGone walls = until (isNothing . findWall walls . fst) (uncurry (walk walls)) . (,[])

walk :: [Wall] -> Guard -> [(Int, Int)] -> (Guard, [(Int, Int)])
walk walls guard walked = (newGuard guard, newWalked guard ++ walked)
  where
    Wall wx wy = fromJust $ findWall walls guard
    newGuard (Guard _ _ Up) = Guard wx (wy - 1) DLeft
    newGuard (Guard _ _ Down) = Guard wx (wy + 1) DRight
    newGuard (Guard _ _ DLeft) = Guard (wx + 1) wy Down
    newGuard (Guard _ _ DRight) = Guard (wx - 1) wy Up
    newWalked (Guard x y Up) = map (x,) [y .. wy - 1]
    newWalked (Guard x y Down) = map (x,) [wy + 1 .. y]
    newWalked (Guard x y DLeft) = map (,y) [wx + 1 .. x]
    newWalked (Guard x y DRight) = map (,y) [x .. wx - 1]

safe :: ([a] -> a) -> [a] -> Maybe a
safe _ [] = Nothing
safe f xs = Just $ f xs

findWall :: [Wall] -> Guard -> Maybe Wall
findWall walls (Guard gx gy Up) =
  safe
    (minimumBy (compare `on` wallY))
    $ filter (\(Wall wx wy) -> gx == wx && wy > gy) walls
findWall walls (Guard gx gy Down) =
  safe
    (maximumBy (compare `on` wallY))
    $ filter (\(Wall wx wy) -> gx == wx && wy < gy) walls
findWall walls (Guard gx gy DRight) =
  safe
    (minimumBy (compare `on` wallX))
    $ filter (\(Wall wx wy) -> gy == wy && wx > gx) walls
findWall walls (Guard gx gy DLeft) =
  safe
    (maximumBy (compare `on` wallX))
    $ filter (\(Wall wx wy) -> gy == wy && wx < gx) walls

finalWalk :: [Wall] -> Guard -> [(Int, Int)] -> [(Int, Int)]
finalWalk walls (Guard gx gy Up) = (++ map (gx,) [gy .. maximum $ map wallY walls])
finalWalk walls (Guard gx gy Down) = (++ map (gx,) [((+ 1) $ minimum $ map wallY walls) .. gy])
finalWalk walls (Guard gx gy DLeft) = (++ map (,gy) [((+ 1) $ minimum $ map wallX walls) .. gx])
finalWalk walls (Guard gx gy DRight) = (++ map (,gy) [gx .. maximum $ map wallX walls])

part2 :: String -> String
part2 input =
  show
    $ length
    $ filter
      (flip walkUntilLoops guard . (: walls))
    $ filter
      (\wall -> wallX wall /= guardX guard && wallY wall /= guardY guard)
      (map (uncurry Wall) $ nub $ uncurry (finalWalk walls) $ walkUntilGone walls guard)
  where
    (walls, guard) = unwrap $ parse grid "" input

walkUntilLoops :: [Wall] -> Guard -> Bool
walkUntilLoops walls initialGuard =
  uncurry elem $
    until
      ( \(guard, guards) ->
          elem guard guards
            || (isNothing . findWall walls) guard
      )
      (uncurry (walkGuards walls))
      (initialGuard, [])

walkGuards :: [Wall] -> Guard -> [Guard] -> (Guard, [Guard])
walkGuards walls guard guards = (newGuard guard, guard : guards)
  where
    Wall wx wy = fromJust $ findWall walls guard
    newGuard (Guard _ _ Up) = Guard wx (wy - 1) DLeft
    newGuard (Guard _ _ Down) = Guard wx (wy + 1) DRight
    newGuard (Guard _ _ DLeft) = Guard (wx + 1) wy Down
    newGuard (Guard _ _ DRight) = Guard (wx - 1) wy Up
