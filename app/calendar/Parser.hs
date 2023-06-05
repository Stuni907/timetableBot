{--
    Dieses File ist aus Lektion 07 fprog kopiert!
--}
module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: String -> Maybe (a, String) }

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else empty

item :: Parser Char
item = P (\n -> case n of
                  (c:cs) -> Just (c,cs)
                  _      -> Nothing)

digit :: Parser Char
digit = sat isDigit

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

instance Functor Parser where
 -- fmap :: (a -> b) -> Parser a -> Parser b
 fmap g p = P (\inp -> case parse p inp of
                         Nothing      -> Nothing
                         Just (v,out) -> Just (g v, out))


instance Applicative Parser where
  -- pure :: a -> Parser a 
  pure v = P (\inp -> Just (v,inp))

  -- <*> :: Parser (a->b) -> Parser a -> Parser b
  pg <*> pa = P (\inp -> 
    case parse pg inp of
      Nothing      -> Nothing
      Just (g,out) -> parse (fmap g pa) out)

string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs



instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> 
    case parse p inp of
      Nothing      -> parse q inp
      Just (v,out) -> Just (v,out)) 

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= q = P (\c -> case parse p c of
                      Nothing -> Nothing
                      Just (v,rest) -> parse (q v) rest)

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol ss = token (string ss)

