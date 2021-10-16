{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Party (Party(..), seats) where

import Data.Aeson
import Data.Aeson.Types
    
data Party =
  Party
    { name :: String
    , votes :: Int
    , initial_seats :: Int
    , extra_seat :: Bool
    }

instance Show Party where
  show p = name p ++ ", " ++ show (seats p) ++ " seats\n"

instance FromJSON Party where
    parseJSON = withObject "Party" $ \v -> Party
        <$> v .: "name"
        <*> v .: "votes"
        <*> pure 0
        <*> pure False

seats :: Party -> Int
seats Party {extra_seat = es, initial_seats = is}
  | es = is + 1
  | otherwise = is