module Ch17 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Bifunctor (class Bifunctor)

test :: Effect Unit
test = do
  log "-- Chapter 17"
  log "applicativeMaybe"
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 25
  log ""

  log "applicativeEither"
  -- LAW: Associative Composition
  -- (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) ==
    (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW: Identity
  -- pure identity <*> x = x
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW: Homomorphism
  -- pure (f x) = pure f <*> pure x
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW: Interchange
  -- u <*> pure x = pure (_ $ x) <*> u
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  log ""

-- 17.1 applicativeMaybe
data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map f (Just x) = Just $ f x
  map _ Nothing = Nothing

instance applyMaybe :: Apply Maybe where
  apply (Just f) x = f <$> x
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left y) _ = Left y

instance applicativeEither :: Applicative (Either a) where
  pure = Right
