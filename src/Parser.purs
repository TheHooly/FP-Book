module Parser where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)


test :: Effect Unit
test = do
  log ""

type ParserState a = Tuple String a
class ParserError (e :: Type) where
  eof :: e
type ParseFunction e a = 
  ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f (Parser g) = Parser \s -> map f <$> g s

instance applyParser :: Apply (Parser e) where
--  Parser f :: Parser e (a -> b)
--  Parser g :: Parser e (a)
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply p1 p2 = Parser \s -> case parse p1 s of
    Left err -> Left err
    Right (Tuple s1 h) -> case parse p2 s1 of
      Left err -> Left err
      Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser p) = p

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Just { head, tail } -> pure $ Tuple tail head
  Nothing -> Left eof

data PError = EOF
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char