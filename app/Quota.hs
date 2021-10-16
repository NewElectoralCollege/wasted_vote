{-# LANGUAGE OverloadedStrings #-}
module Quota (hare, hagenbachBischoff, imperiali, droop, for, Quota(..)) where

import Data.Aeson

import Party

infixr 0 `for`

-- Quotas

data Quota = Traditional (Int -> Int -> Int) | Generous Int (Int -> Int -> Int)

instance FromJSON Quota where
  parseJSON (String a)
    | a == "hare" = pure hare
    | a == "droop" = pure droop
    | a == "hagenbach-bischoff" || a == "hagenbachbischoff" = pure hagenbachBischoff
    | a == "imperiali" = pure imperiali

hare :: Quota
hare =
  Traditional div

droop :: Quota
droop =
  Traditional (\votes seats -> votes `div` (seats + 1) + 1)

hagenbachBischoff :: Quota
hagenbachBischoff =
  Generous 0 (\votes seats -> votes `div` (seats + 1))

imperiali :: Quota
imperiali =
  Generous 0 (\votes seats -> votes `div` (seats + 2))

-- Helpers

proper :: Quota -> Int -> [Party] -> Quota
proper (Generous a b) seats parties
  | awarded == seats = Generous a b
  | otherwise        = Generous (a + 1) b 
  where 
    q = b (sum $ map votes parties) seats
    awarded = sum $ map (\p -> votes p `div` q) parties
proper q _ _ = q

for :: Quota -> Int -> Int -> Int
for quota votes seats =
  case quota of
    Traditional f -> f votes seats
    Generous a f ->  f votes seats + a

