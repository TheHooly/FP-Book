module Ch15 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Int.Bits ((.&.))
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Profunctor (class Profunctor, dimap)
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))
import Data.String (length)

test :: Effect Unit
test = do
  log "-- Chapter 15"
  log ""
  
  log "runPredicate"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log ""

  log "contravariantPredicate"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log ""

  log "runFoldL"
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log ""

  log "profunctorMoore"
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]
  log ""
  log ""  
  
even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) x = f x

-- 15.7 contravariantPredicate
instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

-- 15.8 Moore
data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  dimap f g (Moore s0 output transition) = Moore s0 (g <<< output) (\s -> transition s <<< f)

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

-- 15.16 runFoldL
runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n) addr


