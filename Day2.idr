module Day2

import System.File
import Data.List
import Data.List.Elem
import Data.List1
import Data.Strings
import Data.Bool.Xor
import Common

Chars : Type
Chars = List Char

data Policy = policy Nat Nat Char

parseLine : String -> Maybe (Policy, Chars)
parseLine = parse $
  (,) <$> (policy <$> nat <* char '-' <*> nat <* char ' ' <*> anyChar)
      <*  string ": "
      <*> rest

main : IO ()
main = do
  Right raw <- readFile "./inputs/02"
    | Left err => printLn err

  let items = mapMaybe parseLine (lines raw)

  countValid policy1 items
  countValid policy2 items

  where
    countValid : (Policy -> Chars -> Bool) -> List (Policy, Chars) -> IO ()
    countValid p = printLn . length . filter (uncurry p)

    policy1 : Policy -> Chars -> Bool
    policy1 (policy a b c) pwd = a <= count && count <= b
      where count : Nat
            count = foldr (\x, acc => if x == c then S acc else acc) 0 pwd

    policy2 : Policy -> Chars -> Bool
    policy2 (policy a b c) pwd = xor (at a) (at b)
      where at : Nat -> Bool
            at Z     = False
            at (S k) = case indexElem k pwd of
                       Just (x ** _) => x == c
                       Nothing       => False
