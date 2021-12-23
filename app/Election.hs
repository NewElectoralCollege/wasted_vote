{-# LANGUAGE OverloadedStrings #-}
module Election where

import Data.Aeson

import qualified Data.ByteString.Lazy as B

import Party
import Quota

-- Elections

data Election = Election
    { name :: Maybe String
    , parties :: [Party]
    , seats :: Int
    , quota :: Quota
    }

newtype NECElection = NECElection Election

-- Json

instance FromJSON Election where
    parseJSON = withObject "Election" $ \v -> Election
        <$> v .:? "name"
        <*> v .: "parties"
        <*> v .: "seats"
        <*> v .: "quota"

instance FromJSON NECElection where
    parseJSON = withObject "NECElection" $ \v -> do
        parties <- v .: "parties"
        stats <- v .: "stats"
        seats <- stats .: "total_seats"
        name <- stats .: "name"
        return $ NECElection (Election name parties seats hare)

decodeFileToEither :: Bool -> FilePath -> IO (Either String Election)
decodeFileToEither nec_format file =
    if nec_format
        then do ne <- (eitherDecode <$> B.readFile file) :: IO (Either String NECElection)
                case ne of
                    Left s -> return $ Left s
                    Right (NECElection p) -> return $ Right p
        else eitherDecode <$> B.readFile file
