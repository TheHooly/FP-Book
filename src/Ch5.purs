module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), (-), (==), (>), (<), (>=), (/=), (<<<), (>>>), negate, show, discard, otherwise, max, type (~>))

test :: Effect Unit
test = do
  log "Chapter 5"
  log ""

  log "5.4 flip, 5.5 const"
  log $ show $ flip const 1 2
  log ""

  log "5.11 singleton"
  log $ show $ singleton "xyz"
  log ""

  log "5.12 null"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log ""

  log "5.13 snoc"
  log $ show $ snoc (1 : 2 : Nil) 3
  log ""

  log "5.14 length"
  log $ show $ length (1 : 2 : 3 : Nil)
  log ""

  log "5.16 head"
  log $ show $ (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log ""

  log "5.17 tail"
  log $ show $ (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abc" : "123" : Nil)
  log ""

  log "5.18 last"
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log ""

  log "5.19 init"
  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log ""

  log "5.20 uncons"
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log ""

  log "5.21 index"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log ""

  log "5.22 !!"
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log ""

  log "5.23 findIndex"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log ""

  log "5.24 findLastIndex"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log ""

  log "5.26 reverse"
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log ""

  log "5.27 concat"
  log $ show $
    concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log ""

  log "5.28 filter"
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log ""

  log "5.31 catMaybes"
  log $ show $
    catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log ""

  log "5.32 range"
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log ""

  log "5.34 take"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log ""

  log "5.35 drop"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log ""

  log "5.36 takeWhile"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log ""

  log "5.37 dropWhile"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil) 
  log ""

  log "5.38 takeEnd"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log ""

  log "5.40 dropEnd"
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log ""

  log "5.41 zip"
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log ""

  log "5.42 unzip"
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))
  log ""

-- 5.4
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- 5.5
const :: forall a b. a -> b -> a
const x _ = x

-- 5.7
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

-- 5.8
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

-- 5.11
singleton :: forall a. a -> List a
singleton x = x : Nil

-- 5.12
null :: forall a. List a -> Boolean
null Nil  = true
null _    = false

-- 5.13
snoc :: forall a. List a -> a -> List a
snoc Nil x        = singleton x
snoc (y : ys) x   = y : snoc ys x

-- 5.14
length :: forall a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil        = acc
  go acc (_ : xs)   = go (acc + 1) xs

-- 5.16
head :: forall a. List a -> Maybe a
head Nil      = Nothing
head (x : _)  = Just x

-- 5.17
tail :: forall a. List a -> Maybe (List a)
tail Nil        = Nothing
tail (_ : xs)   = Just xs

-- 5.18
last :: forall a. List a -> Maybe a
last Nil        = Nothing
last (x : Nil)  = Just x
last (_ : xs)   = last xs

-- 5.19
init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go Nil        = Nil
  go (_ : Nil)  = Nil
  go (x : xs)   = x : go xs

-- 5.20
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil        = Nothing
uncons (x : xs)   = Just { head : x, tail : xs }

-- 5.21
index :: forall a. List a -> Int -> Maybe a
index Nil _       = Nothing
index (x : _) 0   = Just x
index (_ : xs) i  = index xs (i - 1)

-- 5.22
infixl 8 index as !!

-- 5.23
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred l = go 0 l where
  go _ Nil        = Nothing
  go i (x : xs)   = if pred x then Just i else go (i + 1) xs

-- 5.24
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred l = go Nothing 0 l where
  go fi _ Nil       = fi
  go fi i (x : xs)  = go (if pred x then Just i else fi) (i + 1) xs

-- 5.26
reverse :: List ~> List
reverse Nil = Nil
reverse ol = go Nil ol where
  go rl Nil       = rl
  go rl (x : xs)  = go (x : rl) xs

-- 5.27
concat :: forall a. List (List a) -> List a
concat Nil                = Nil
concat (Nil : xss)        = concat xss
concat ((x : xs) : xss)   = x : concat (xs : xss)

-- 5.28
filter :: forall a. (a -> Boolean) -> List a -> List a
filter pred = reverse <<< go Nil where
  go nl Nil       = nl
  go nl (x : xs)  = go (if pred x then x : nl else nl) xs

-- 5.31
catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y    -> y : catMaybes xs
  Nothing   -> catMaybes xs

-- 5.32
range :: Int -> Int -> List Int
range start end
  | start == end  = singleton start
  | otherwise     =
    start : range (start + (if start < end then 1 else (-1))) end

-- 5.34
take :: forall a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n) where
  go nl _ Nil         = nl
  go nl 0 _           = nl
  go nl n' (x : xs)   = go (x : nl) (n' - 1) xs

-- 5.35
drop :: forall a. Int -> List a -> List a
drop n = go (max 0 n) where
  go 0 l          = l
  go _ Nil        = Nil
  go n' (_ : xs)  = drop (n' - 1) xs

-- 5.36
takeWhile :: forall a . (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

-- 5.37
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

-- 5.38
takeEnd :: forall a. Int -> List a -> List a
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then x : nl else nl

-- 5.40
dropEnd :: forall a. Int -> List a -> List a
dropEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) if c < n then nl else x : nl

-- 5.41
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

-- 5.42
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) =
  unzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)