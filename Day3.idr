module Day3

import System.File
import Data.Vect as Vect
import Data.List
import Data.Strings
import Data.Fin
import Common

data Tile = Tree | Empty

Grid : Type
Grid = List (Vect 31 Tile)

count : Nat -> Integer -> Grid -> Nat -> Integer -> Integer
count _     _ []        _     _  = 0
count _     _  _        Z     _  = 0
count (S n) x (y :: ys) dy    dx = count n x ys dy dx
count Z     x (y :: ys) (S n) dx =
    count n (x + dx) ys (S n) dx
    + case index (restrict 30 x) y of
           Tree  => 1
           Empty => 0

main : IO ()
main = do
  Right raw <- readFile "./inputs/03"
    | Left err => printLn err

  let Just grid = parse gridP raw
    | Nothing => printLn "Invalid input"

  let counts = uncurry (count 0 0 grid) <$> items

  printLn (head counts)
  printLn (product counts)

  where
      toTile : Char -> Maybe Tile
      toTile '.' = Just Empty
      toTile '#' = Just Tree
      toTile _   = Nothing

      gridP : Parser Maybe Grid
      gridP = sepBy (char '\n') (repeat 31 (guardM toTile anyChar)) <* option (char '\n')

      items : Vect 5 (Nat, Integer)
      items = [(1, 3), (1, 1), (1, 5), (1, 7), (2, 1)]
