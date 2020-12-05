module Day5

import System.File
import Data.Strings
import Data.Nat
import Data.Vect
import Data.Vect.Sort
import Data.Fin
import Common


main : IO ()
main = do
  Right raw <- readFile "./inputs/05"
    | Left err => printLn err

  let Just ids = parse (sepBy (char '\n') seatP <* option (char '\n')) raw
    | Nothing => printLn "Invalid input"

  printLn $ maximum 0 ids
  printLn $ findSeat $ sort $ fromList ids

  where
      findSeat : Vect n Integer -> Integer
      findSeat (l :: h :: t) = if l >= 7 && h - l == 2 then l + 1
                                                       else findSeat (h :: t)
      findSeat _ = 0

      seatP : (Monad m, Alternative m) => Parser m Integer
      seatP = foldl (\x, y => 2 * x + y) 0 <$> repeat 10 (guardM fb anyChar)
        where fb : Char -> Maybe Integer
              fb 'F' = Just 0
              fb 'B' = Just 1
              fb 'L' = Just 0
              fb 'R' = Just 1
              fb _   = Nothing
