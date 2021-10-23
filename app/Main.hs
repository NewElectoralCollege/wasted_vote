{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Exit as Exit
import qualified System.Environment as Env
import qualified System.FilePath as FP

import Data.Aeson
import Data.String

import qualified Data.ByteString.Lazy as B

import Lrm
import Brute
import qualified Output as O
import Party
import Quota
import Control.Monad

version = "0.1"

main :: IO ()
main = 
    do args <- Env.getArgs
       handleArgs (Write . show) args
       Exit.exitSuccess

-- Elections

data Election = Election
    { parties :: [Party]
    , seats :: Int
    , quota :: Quota
    }

instance FromJSON Election where
    parseJSON = withObject "Election" $ \v -> Election
        <$> v .: "parties"
        <*> v .: "seats"
        <*> v .: "quota"

-- Arguments

data Argument
    = Write String
    | Location String
    | Save String B.ByteString
    | Fail String

identifyArg :: (O.Output -> Argument) -> String -> IO Argument
identifyArg retfunc arg =
    case arg of
        "--help" ->
            return $ Write $ unlines 
                [ "-------------------------------------------------------------------------------"
                , "Thank You for using this program!"
                , "  x   --help                 Display this message."
                , "  x   --version              Show the version of the program."
                , "  x   --example              Creates an example JSON file."
                , "  x   --out=*.json           Writes results to a JSON file."
                , "  x   *.json                 Run program with JSON file."
                , ""
                , "Source:"
                , "  https://github.com/NewElectoralCollege/wasted_vote"
                , ""
                , "Copyright info:"
                , "  Copyright 2021 - The New Electoral College"
                , "-------------------------------------------------------------------------------"
                ]

        "--version" ->
            return $ Write version

        "--example" ->
            return $ Save "example.json"
                "[\n\
                \  { \"name\": \"Party A\", \"votes\": 100000 },\n\
                \  { \"name\": \"Party B\", \"votes\": 80000 },\n\
                \  { \"name\": \"Party C\", \"votes\": 30000 },\n\
                \  { \"name\": \"Party D\", \"votes\": 20000 }\n\
                \]"
        
        a ->
            if take 6 a == "--out=" 
                then return $ Location $ drop 6 a
                else if FP.takeExtension a == ".json" then
                    do ri <- ((eitherDecode <$> B.readFile a) :: IO (Either String Election))
                       case ri of
                            Left s ->
                                return $ Fail ("The following error was returned: " ++ s)
                
                            Right p ->
                                return $ retfunc (brute (quota p) (parties p) (Main.seats p))
                else return $ Fail ("I can't identify this argument: " ++ a)


handleArgs :: (O.Output -> Argument) -> [String] -> IO ()
handleArgs _ [] = Exit.exitFailure
handleArgs retfunc (x:xs) = 
    do r <- identifyArg retfunc x
       case r of
            Write s -> putStrLn s
            Location l -> handleArgs (Save l . encode) xs
            Save f t -> B.writeFile f t
            Fail f -> do putStrLn f
                         Exit.exitFailure

