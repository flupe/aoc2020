module Day4

import System.File
import Data.Vect
import Data.List
import Data.Strings
import Data.Maybe
import Common


Pass : Type
Pass = List (String, String)

isSep : Char -> Bool
isSep c = c == ' ' || c == '\n'

kv : Parser Maybe (String, String)
kv = (,) <$> (pack . toList <$> repeat 3 anyChar)
         <*> (char ':' *> pack <$> many (guard (not . isSep) anyChar))

parseInput : String -> Maybe (List Pass)
parseInput =
    parse (sepBy (string "\n\n") (sepBy (guard isSep anyChar) kv)
              <* option (char '\n'))

requiredKeys : List String
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

allowedCols : List String
allowedCols = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

rangeNat :  String -> Nat -> Nat -> Bool
rangeNat s a b = fromMaybe False (map (\c => a <= c && c <= b) (parse nat s))

validate : String -> String -> Bool
validate "byr" c = rangeNat c 1920 2002
validate "iyr" c = rangeNat c 2010 2020
validate "eyr" c = rangeNat c 2020 2030
validate "hgt" c = case parse ((,) <$> nat <*> repeat 2 anyChar) c of
                        Just (x, ['c', 'm']) => 150 <= x && x <= 193
                        Just (x, ['i', 'n']) => 59 <= x  && x <= 76
                        Nothing              => False
validate "hcl" c = isJust (parse (char '#' *> repeat 6 (oneOf (unpack "0123456789abcdef") anyChar)) c)
validate "ecl" c = any (==c) allowedCols
validate "pid" c = isJust (parse (repeat 9 digit) c)
validate "cid" _ = True
validate _     _ = False


isValid1 : Pass -> Bool
isValid1 p = all (isJust . flip lookup p) requiredKeys

isValid2 : Pass -> Bool
isValid2 = all (uncurry validate)


main : IO ()
main = do
  Right raw <- readFile "./inputs/04"
    | Left err => printLn err

  let Just passports = parseInput raw
    | Nothing => printLn "Invalid input"

  let valid1 = filter isValid1 passports

  printLn (length valid1)
  printLn (length $ filter isValid2 valid1)
