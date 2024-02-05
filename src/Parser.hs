module Parser where

import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Witherable (Filterable, mapMaybe)
import qualified Witherable as W
import Control.Applicative (many)
import GHC.Base (Alternative ((<|>)))
import Options.Applicative (empty)

newtype ParserT m s a = Parser
  { parse :: s -> m (s, a)
  }


type Parser = ParserT Maybe String

instance Functor Parser where
  fmap f p = Parser $ fmap (fmap f) . parse p

instance Filterable Parser where
  mapMaybe f p = Parser $ W.mapMaybe (traverse f). parse p

instance Applicative Parser where
  pure a = Parser $ \s -> Just (s, a)
  p1 <*> p2 = Parser $ \s -> do
    (s1, f) <- parse p1 s
    (s2, a) <- parse p2 s1
    pure (s2, f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> parse p1 s <|> parse p2 s


operationP :: Parser Char
operationP = char '+' <|> char '*'

anyChar :: Parser Char
anyChar = Parser p where
  p [] = Nothing
  p (h:t) = Just (t, h)

parseAll :: Parser a -> String -> Maybe a
parseAll p = fmap snd . W.filter (null . fst) . parse p

char :: Char -> Parser Char
char c = W.filter (== c) anyChar

digit :: Parser Char
digit = W.filter isDigit anyChar

repeat1 :: Parser a -> Parser [a]
repeat1 = many

digits :: Parser Integer
digits = fmap read (repeat1 digit)

decimal :: Parser Double
decimal = f <$> digits <*> char '.' <*> digits
 where
  f i _ d = read (show i ++ ['.'] ++ show d)

string :: String -> Parser String
string = traverse char

parenthesised :: Parser a -> Parser a
parenthesised p = char '(' *> p <* char ')'
