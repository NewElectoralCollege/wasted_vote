{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Lrm (Party(..), seats, extraVotes, totalSeats, totalVotes, hareMethod) where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Ord

-- Party

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


-- Calculation

seats :: Party -> Int
seats Party {extra_seat = es, initial_seats = is}
  | es = is + 1
  | otherwise = is

extraVotes :: Int -> Party -> Int
extraVotes quota party = votes party - (initial_seats party * quota)

totalVotes :: [Party] -> Int
totalVotes list = sum $ map votes list

totalSeats :: [Party] -> Int
totalSeats list = sum $ map seats list

quotaSeats :: Int -> [Party] -> [Party]
quotaSeats _ [] = []
quotaSeats quota (x:xs) =
  x {initial_seats = votes x `div` quota, extra_seat = False} : quotaSeats quota xs

extraSeats :: Int -> ([Party], [Party]) -> [Party]
extraSeats _ (l, []) = l
extraSeats s (done, todo@(x:xs)) =
  if sum (map seats (done ++ todo)) == s
    then done ++ todo
    else extraSeats s (done ++ [x {extra_seat = True}], xs)

tp :: [a] -> ([a], [a])
tp a = ([], a)

hareMethod :: Int -> [Party] -> [Party]
hareMethod _ [] = error "No Parties provided"
hareMethod 0 _ = error "At least one seat must be awarded"
hareMethod s list =
  extraSeats s $
  tp $ sortOn (Down . extraVotes quota) $ quotaSeats quota list
  where
    quota = totalVotes list `div` s

