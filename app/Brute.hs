module Brute (brute) where

import Lrm
import Party
import Quota

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

brute :: Quota -> [Party] -> Int -> [WastedVote]
brute quota p1 seats = 
       map fm p2
       where
           p2 = if totalSeats p1 == 0
                    then lrm quota seats p1
                    else p1
           fm = bruteParty quota p2 seats


bruteParty :: Quota -> [Party] -> Int -> Party -> WastedVote
bruteParty quota parties seats p =
    if Party.seats p == 0
        then noSeats p
        else withSeats quota parties seats p


updateParty :: [Party] -> Party -> Int -> [Party]
updateParty parties party votes =
    party { votes = votes } :
    filter ((/=) (Party.name party) . Party.name) parties

noSeats :: Party -> WastedVote
noSeats p =
    WastedVote (Party.name p) (votes p) 0

withSeats :: Quota -> [Party] -> Int -> Party -> WastedVote
withSeats quota parties seats p =
        WastedVote (Party.name p) (Party.votes p - nwv) nwv
    where
        s = show (votes p)
        d reduction votes
            | Party.seats np == Party.seats p = d reduction (votes - reduction)
            | reduction == 1 = votes
            | otherwise = d (reduction `div` 10) (votes + reduction)
            where
                nps = lrm quota seats $ updateParty parties p votes
                np = fromMaybe p $ find ((==) (Party.name p) . Party.name) nps
        ir = 10 ^ (length s - 1)
        nwv = d ir (Party.votes p)
        