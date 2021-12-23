{-# LANGUAGE OverloadedStrings #-}
module Output where
    
import Quota
import WastedVote
import Lrm

import qualified Party as P

import Data.Aeson

-- Json

data Output = Output
    { name :: Maybe String
    , wasted_vote :: [WastedVote]
    , parties :: [P.Party]
    , seats :: Int
    , quota :: Quota
    }

-- Printing

instance Show Output where
    show o = unlines 
        [ "Name:"
        , show $ Output.name o
        , "Seats:" 
        , show $ parties o
        , "\n"
        , "Wasted Votes:"
        , show $ wasted_vote o
        , "\n"
        , "Total Seats: " ++ show (seats o)
        , "Quota: " ++ show (makeQuota o)
        ]

-- JSON

instance ToJSON Output where
    toJSON o = object 
        [ "name" .= Output.name o
        , "wasted_vote" .= wasted_vote o
        , "parties" .= parties o
        , "seats" .= seats o
        , "quota" .= makeQuota o
        ]

-- Misc

makeQuota :: Output -> Int
makeQuota o =
    (quota o `for` totalVotes (parties o)) (seats o)
