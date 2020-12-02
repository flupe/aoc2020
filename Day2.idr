module Day2

import System.File
import Data.List
import Data.List.Elem
import Data.List1
import Data.Strings
import Data.Bool.Xor

Chars : Type
Chars = List Char

data Policy = policy Char Nat Nat

-- todo: make total

partial
parseLine : String -> (Policy, Chars)
parseLine line =
  let chars = unpack line in
  let (popo  ::: [' ' :: word]) = splitOn ':' chars in
  let (range ::: [[c]])         = splitOn ' ' popo  in
  let (a     ::: b :: [])       = splitOn '-' range in
  (policy c (toN a) (toN b), word)
  where toN : Chars -> Nat
        toN = stringToNatOrZ . pack

partial
main : IO ()
main = do
  Right raw <- readFile "./inputs/02"
    | Left err => printLn err

  let items = map parseLine (lines raw)

  countValid policy1 items
  countValid policy2 items

  where
    countValid : (Policy -> Chars -> Bool) -> List (Policy, Chars) -> IO ()
    countValid p = printLn . length . filter (uncurry p)

    policy1 : Policy -> Chars -> Bool
    policy1 (policy c a b) pwd = a <= count && count <= b
      where count : Nat
            count = foldr (\x, acc => if x == c then S acc else acc) 0 pwd

    policy2 : Policy -> Chars -> Bool
    policy2 (policy c a b) pwd = xor (at a) (at b)
      where at : Nat -> Bool
            at Z     = False
            at (S k) = case indexElem k pwd of
                       Just (x ** _) => x == c
                       Nothing       => False
