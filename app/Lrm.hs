module Lrm (extraVotes, totalSeats, totalVotes, lrm) where

import Data.List
import Data.Ord

import Quota
import Party


-- Calculation

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

lrm :: Quota -> Int -> [Party] -> [Party]
lrm qt _ [] = error "No Parties provided"
lrm qt 0 _ = error "At least one seat must be awarded"
lrm qt s list =
  extraSeats s $
  tp $ sortOn (Down . extraVotes quota) $ quotaSeats quota list
  where
    tv = totalVotes list
    quota = (hare `for` tv) s

