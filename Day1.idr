module Day1

import Data.Nat
import Data.List.Elem
import Data.Vect.Quantifiers
import Decidable.Equality
import System.File
import Data.Strings
import Data.Vect


ElemOf : List a -> a -> Type
ElemOf = flip Elem


record Result (inputs : List Nat) (size, target : Nat) where
  constructor MkResult
  values      : Vect size Nat
  fromInput   : All (ElemOf inputs) values
  sumToTarget : sum values === target


stepFold : (xs : Vect n a) -> foldrImpl f e go xs === go (foldrImpl f e Prelude.id xs)
stepFold []        = Refl
stepFold (x :: xs) = trans (stepFold xs) (cong go (sym (stepFold xs)))


find : (s, t : Nat) ->
       (xs : List Nat) ->
       (forall x. Elem x xs -> Elem x i) ->
       Maybe (Result i s t)
find _     _ []  _ = Nothing
find 0     t xs  f = Nothing
find 1     t xs  f =
    case isElem t xs of
        Yes e => Just (MkResult [t] [f e] (plusZeroRightNeutral t))
        No  _ => Nothing
find (S (S s)) t (x :: xs) f =
    case cmp x t of
         CmpLT y => case find (S s) (S y) xs (f . There) of
              Just r  => Just (MkResult (x      :: r.values)
                                        (f Here :: r.fromInput)
                                        (trans (stepFold _) (cong (plus x) (r.sumToTarget))))
              Nothing => find (S (S s)) (x + S y) xs (f . There)
         _       => find (S (S s)) t xs (f . There)


findNuple : (s, t : Nat) -> (xs : List Nat) -> Maybe (Result xs s t)
findNuple s t xs = find s t xs id


main : IO ()
main = do
  Right raw <- readFile "./inputs/01"
    | Left err => putStrLn (show err)

  let input = map stringToNatOrZ (lines raw)

  showRes (findNuple 2 2020 input)
  showRes (findNuple 3 2020 input)

  where
      showRes : Maybe (Result i s t) -> IO ()
      showRes = printLn . map (product . values)
