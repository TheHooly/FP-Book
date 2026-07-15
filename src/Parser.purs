module Parser where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate)
import Control.Plus (class Plus, empty)
import Effect (Effect)
import Effect.Console (log)


test :: Effect Unit
test = do
  log "-- Parser"
  log ""

  log "parse char twoChars threeChars"
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log ""

  log "parse threeChars too short"
  log $ show $ parse' threeChars "A"
  log ""

  log "parse fromCharArray count"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log ""
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

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' (Parser p) = p

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

threeChars :: ∀ e. Parser e String
threeChars = 
  (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

count 
  :: ∀ e a f
  . Plus f 
  => Traversable f 
  => Unfoldable f 
  => Int 
  -> Parser e a 
  -> Parser e (f a)
count n p
  | n <= 0 = pure empty
  | otherwise = sequence (replicate n p)