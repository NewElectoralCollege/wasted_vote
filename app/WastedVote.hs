{-# LANGUAGE OverloadedStrings #-}
module WastedVote (WastedVote(..)) where

import qualified Party as Brute

import Data.Aeson

data WastedVote = WastedVote
    { name :: String
    , wasted :: Int
    , represented :: Int
    }

instance Show WastedVote where
    show wv = name wv ++ ": " ++ show (wasted wv) ++ "/" ++ show (represented wv) ++ "\n"

instance ToJSON WastedVote where
    toJSON wv = object 
        [ "name" .= name wv
        , "wasted" .= wasted wv
        , "represented" .= represented wv
        ]

total :: WastedVote -> Int
total (WastedVote {wasted = wasted,represented = represented}) =
    wasted + represented


-- (%) :: WastedVote -> Float
-- (%) wv =
--     fromInt $ (wasted wv) `div` (total wv)