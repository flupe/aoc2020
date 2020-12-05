module Day5

import System.File
import Data.Strings
import Data.Nat
import Data.Vect
import Data.Vect.Sort
import Data.Fin
import Common


Seat : Type
Seat = (Fin 128, Fin 8)

data Dir  = L | R

toIndex : {n : Nat} -> Vect n Dir -> Fin (power 2 n)
toIndex {n = Z}   []       = FZ
toIndex {n = S n} (L :: t) =
    weakenLTE (toIndex t) (lteTransitive (lteAddRight {m = Z} (power 2 n))
                                         (plusLteMonotoneLeft _ _ _ LTEZero))

toIndex {n = S n} (R :: t) =
  rewrite plusZeroRightNeutral (power 2 n) in shift (power 2 n) (toIndex t)


seatP : (Monad m, Alternative m) => Parser m Seat
seatP =
  (,) <$> (toIndex <$> repeat 7 (guardM fb anyChar))
      <*> (toIndex <$> repeat 3 (guardM fb anyChar))
  where fb : Char -> Maybe Dir
        fb 'F' = Just L
        fb 'B' = Just R
        fb 'L' = Just L
        fb 'R' = Just R
        fb _   = Nothing

main : IO ()
main = do
  Right raw <- readFile "./inputs/05"
    | Left err => printLn err

  let Just ids = map seatID <$> parse (sepBy (char '\n') seatP <* option (char '\n')) raw
    | Nothing => printLn "Invalid input"

  printLn $ maximum 0 ids
  printLn $ findSeat (sort (fromList ids))

  where
      seatID : Seat -> Integer
      seatID (r, c) = finToInteger r * 8 + finToInteger c

      findSeat : Vect n Integer -> Integer
      findSeat (l :: h :: t) = if l >= 7 && h - l == 2 then l + 1
                                                       else findSeat (h :: t)
      findSeat _ = 0
