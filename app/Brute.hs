module Brute (brute) where

import Lrm
import Party
import Quota
import WastedVote
import Output

import Data.List
import Data.Maybe


-- Process

brute :: Maybe String -> Quota -> [Party] -> Int -> Output
brute name quota p1 seats = 
    Output name wv p2 seats quota
    where
        p2 = if totalSeats p1 == 0
                then lrm quota seats p1
                else p1
        fm = bruteParty quota p2 seats
        wv = map fm p2


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
        