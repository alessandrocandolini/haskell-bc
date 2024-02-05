{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Grammar where

import Parser
import Control.Applicative ((<|>))

data Expr a
  = Literal a
  | Add (Expr a) (Expr a)
  | Multiply (Expr a) (Expr a)
  deriving (Eq, Show)

evaluate :: Expr Int -> Int
evaluate (Literal a) = a
evaluate (Add expr1 expr2) = evaluate expr1 + evaluate expr2
evaluate (Multiply expr1 expr2) = evaluate expr1 * evaluate expr2

expr :: Parser (Expr Double)
expr = fmap Literal decimal <|> parenthesised p where
   p = Add <$> expr <* char '+' <*> expr

