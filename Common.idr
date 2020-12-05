module Common

import Data.List
import Data.List1
import Data.Vect

%default total

public export
record Parser (m : Type -> Type) (a : Type) where
    constructor MkParser
    runParser : List Char -> m (a, List Char)

public export
void : Applicative m => m a -> m ()
void x = x *> pure ()

public export
option : (Alternative m) => m a -> m (Maybe a)
option p = (Just <$> p) <|> pure Nothing

public export
Functor m => Functor (Parser m) where
  map f p = MkParser run
    where run : List Char -> m (b, List Char)
          run s = mapFst f <$> (runParser p s)

public export
Monad m => Applicative (Parser m) where
  pure x = MkParser (pure . (x,) . id)
  pf <*> px = MkParser \s => do (f, o)  <- runParser pf s
                                (x, o') <- runParser px o
                                pure (f x, o')

public export
Monad m => Monad (Parser m) where
  pf >>= f = MkParser \s => do (x, o) <- runParser pf s
                               runParser (f x) o

public export
anyChar : (Alternative m, Applicative m) => Parser m Char
anyChar = MkParser run
    where run : List Char -> m (Char, List Char)
          run []        = empty
          run (x :: xs) = pure (x , xs)

public export
guardM : (Alternative m, Monad m) => (a -> Maybe b) -> Parser m a -> Parser m b
guardM f p = MkParser \s => do (x, o) <- runParser p s
                               case f x of
                                    Nothing => empty
                                    Just y  => pure (y, o)

public export
guard : (Alternative m, Monad m) => (a -> Bool) -> Parser m a -> Parser m a
guard f p = MkParser \s => do (x, o) <- runParser p s
                              if f x then pure (x, o)
                                     else empty

public export
oneOf : (Alternative m, Monad m, Eq a) =>  List a -> Parser m a -> Parser m a
oneOf l p = guard (\c => elemBy (==) c l) p

public export
char : (Alternative m, Monad m) => Char -> Parser m Char
char c = guard (==c) anyChar

public export
chars : (Alternative m, Monad m) => List Char -> Parser m (List Char)
chars [] = pure []
chars (x :: xs) = (::) <$> char x <*> chars xs

public export
string : (Alternative m, Monad m) => String -> Parser m (String)
string s = chars (unpack s) *> pure s

public export
parse : (Alternative m, Monad m) => Parser m a -> String -> m a
parse p s = runParser p (unpack s) >>= \c => case c of (r, []) => pure r
                                                       _       => empty

public export
digit : (Alternative m, Monad m) => Parser m Nat
digit = guardM toDigit anyChar
  where toDigit : Char -> Maybe Nat
        toDigit '0' = Just 0
        toDigit '1' = Just 1
        toDigit '2' = Just 2
        toDigit '3' = Just 3
        toDigit '4' = Just 4
        toDigit '5' = Just 5
        toDigit '6' = Just 6
        toDigit '7' = Just 7
        toDigit '8' = Just 8
        toDigit '9' = Just 9
        toDigit _   = Nothing

public export
(Alternative m, Monad m) => Alternative (Parser m) where
  p <|> q = MkParser \s => runParser p s <|> runParser q s

public export
rest : Applicative m => Parser m (List Char)
rest = MkParser (pure . (,[]))

public export
fail : (Alternative m) => Parser m a
fail = MkParser (const empty)

public export
repeat : Monad m => (n : Nat) -> Parser m a -> Parser m (Vect n a)
repeat Z     p = pure []
repeat (S n) p = (::) <$> p <*> repeat n p

public export
partial
many : (Alternative m, Monad m) => Parser m a -> Parser m (List a)
many p = ((::) <$> p <*> many p) <|> pure []

public export
partial
some : (Alternative m, Monad m) => Parser m a -> Parser m (List1 a)
some p = (:::) <$> p <*> many p

try : (Alternative m, Monad m) => Parser m a -> Parser m a


public export
partial
nat : (Alternative m, Monad m) => Parser m Nat
nat = do (x ::: xs) <- some digit
         pure (foldl (\x, y => x * 10 + y) x xs)

public export partial
sepBy : (Alternative m, Monad m) => Parser m a -> Parser m b -> Parser m (List b)
sepBy sep p = ((::) <$> p <*> ((sep *> sepBy sep p) <|> pure []))
