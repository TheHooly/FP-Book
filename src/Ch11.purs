module Ch11 where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semiring (class Semiring, zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude ((+), (<>), (<<<), discard, negate, class Ord, type (~>), Unit, show, ($), (>), otherwise)

test :: Effect Unit
test = do
  log "-- Chapter 11"
  log ""
  log "fold reverse"
  log $ show $ reverse $ (10 : 20 : 30 : Nil)
  log ""

  log "fold findMax"
  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log ""

  log "fold findMaxNE"
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log ""

  log "foldable Tree"
  log $ show $ sum
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log ""
  log ""

reverse :: List ~> List
reverse = foldl (\rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y 
  | x > y = x 
  | otherwise = y

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (x :| xs) = foldl f x xs

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

-- Foldable Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
toList (Node lt rt) = toList lt <> toList rt

instance foldableTree :: Foldable Tree where
  foldr f acc = foldr f acc <<< toList
  foldl f acc = foldl f acc <<< toList
  foldMap f = foldMap f <<< toList