module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, show, discard, ($), (*), (/), flip, identity)

test :: Effect Unit
test = do
  log "-- Chapter 13"
  log ""
  log "functorMaybe"
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log ""

  log "functorEither"
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 1) <$> Left "Error reason"
  log ""

  log "functorThreeple"
  log $ show $ (_ / 2) <$> Threeple 10 20 30
  log ""

  log "bifunctorEither"
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
  log ""

  log "bifunctorTuple"
  log $ show $ rmap (_ * 2) $ Tuple 80 40
  log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
  log ""

  log "bifunctorThreeple"
  log $ show $ rmap (_ * 2) $ Threeple 99 80 40
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40
  log ""
  log ""

-- functorMaybe
data Maybe a = Nothing | Just a

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b
infixl 4 map as <$>

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

-- functorEither
data Either a b = Left a | Right b
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b)
  where show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left err) = Left err
  map f (Right x) = Right $ f x

-- functorTuple
data Tuple a b = Tuple a b
derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b)
  where show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x (f y)

data Threeple a b c = Threeple a b c
derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c)
  where show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y (f z)

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> b) -> (c -> d) -> f a c -> f b d

rmap :: forall f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: forall f a b c. Bifunctor f => (a -> b) -> f a c -> f b c 
lmap = flip bimap identity

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left err) = Left $ f err
  bimap _ g (Right x) = Right $ g x

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap f g (Threeple x y z) = Threeple x (f y) (g z)
