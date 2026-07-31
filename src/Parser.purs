module Parser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.String.CodePoints (codePointFromChar)
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
  log "-- Applicative Parser"
  log ""

  log "parse char twoChars threeChars"
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoCharsA "ABC"
  log $ show $ parse' threeCharsA "ABC"
  log ""

  log "parse threeChars too short"
  log $ show $ parse' threeCharsA "A"
  log ""

  log "parse fromCharArray count"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log ""

  log "-- Monadic Parser"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######" 

type ParserState a = Tuple String a
class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

type ParseFunction e a = 
  ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f (Parser g) = Parser \s -> map f <$> g s

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply = ap
  -- apply p1 p2 = Parser \s -> do
    -- Tuple s1 f <- parse p1 s
    -- Tuple s2 x <- parse p2 s1
    -- pure $ Tuple s2 $ f x
  -- apply p1 p2 = Parser \s -> case parse p1 s of
    -- Left err -> Left err
    -- Right (Tuple s1 h) -> case parse p2 s1 of
      -- Left err -> Left err
      -- Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance bindParser :: Bind (Parser e) where
  bind p f = Parser \s -> do
    Tuple s1 x <- parse p s
    parse (f x) s1

instance monadParser :: Monad (Parser e)

-- ap :: ∀ m a b. Monad m => m (a -> b) -> m a -> m b
-- ap mf mx = do
  -- f <- mf
  -- x <- mx
  -- pure $ f x

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser p) = p

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' (Parser p) = p

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Just { head, tail } -> pure $ Tuple tail head
  Nothing -> Left eof

data PError = EOF | InvalidChar String
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsM :: ∀ e. Parser e (Tuple Char Char)
twoCharsM = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = 
  (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

threeCharsM :: ∀ e. Parser e String
threeCharsM = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1, c2, c3]

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

satisfy 
  :: ∀ e
  . ParserError e 
  => String 
  -> (Char -> Boolean) 
  -> Parser e Char
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

instance altParser :: Alt (Parser e) where
  alt p1 p2 = Parser \s -> case parse p1 s of
    Right x -> Right x
    Left _ -> parse p2 s

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p