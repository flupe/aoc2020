module Day1

import Data.Nat
import System.File
import Data.Strings


findMatch : (depth : Nat) -> (target : Nat) -> List Nat -> Maybe Nat
findMatch Z Z _  = Just 1
findMatch Z _ _  = Nothing
findMatch d t [] = Nothing
findMatch (S d) t (x :: xs) =
  if x <= t then
    case findMatch d (minus t x) xs of
         Just y  => Just (x * y)
         Nothing => findMatch (S d) t xs
  else findMatch (S d) t xs


main : IO ()
main = do
  Right raw <- readFile "./inputs/01"
    | Left err => putStrLn (show err)

  let input = map stringToNatOrZ (lines raw)

  print (findMatch 2 2020 input)
  print (findMatch 3 2020 input)
