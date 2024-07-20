module Ch7a where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=))

import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)
import Data.Ord (class Ord, Ordering(..), compare)

data Maybe a = Nothing | Just a

test :: Effect Unit
test = do
  log $ "-- 7.5 eqMaybe"
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log $ ""

  log $ "-- 7.6 ordMaybe"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log ""



instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing       = EQ
  compare (Just x) (Just y)     = compare x y
  compare Nothing _             = LT
  compare _ Nothing             = GT