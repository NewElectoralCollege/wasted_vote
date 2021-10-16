module Brute (brute) where

import Lrm

import Data.List
import Data.Maybe


-- Wasted Vote type

data WastedVote = WastedVote
    { name :: String
    , wasted :: Int
    , represented :: Int
    }

instance Show WastedVote where
    show wv = Brute.name wv ++ ": " ++ show (wasted wv) ++ "/" ++ show (represented wv) ++ "\n"

total :: WastedVote -> Int
total (WastedVote {wasted = wasted,represented = represented}) =
    wasted + represented


-- (%) :: WastedVote -> Float
-- (%) wv =
--     fromInt $ (wasted wv) `div` (total wv)

-- Process

brute :: [Party] -> Int -> [WastedVote]
brute p1 seats = 
       map fm p2
       where
           p2 = if totalSeats p1 == 0
                    then hareMethod seats p1
                    else p1
           fm = bruteParty p2 seats


bruteParty :: [Party] -> Int -> Party -> WastedVote
bruteParty parties seats p =
    if Lrm.seats p == 0
        then noSeats p
        else withSeats parties seats p


updateParty :: [Party] -> Party -> Int -> [Party]
updateParty parties party votes =
    party { votes = votes } :
    filter ((/=) (Lrm.name party) . Lrm.name) parties

noSeats :: Party -> WastedVote
noSeats p =
    WastedVote (Lrm.name p) (votes p) 0

withSeats :: [Party] -> Int -> Party -> WastedVote
withSeats parties seats p =
        WastedVote (Lrm.name p) (Lrm.votes p - nwv) nwv
    where
        s = show (votes p)
        d reduction votes
            | Lrm.seats np == Lrm.seats p = d reduction (votes - reduction)
            | reduction == 1 = votes
            | otherwise = d (reduction `div` 10) (votes + reduction)
            where
                nps = hareMethod seats $ updateParty parties p votes
                np = fromMaybe p $ find ((==) (Lrm.name p) . Lrm.name) nps
        ir = 10 ^ (length s - 1)
        nwv = d ir (Lrm.votes p)
        