{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.Exit as Exit
import qualified System.Environment as Env
import qualified System.FilePath as FP

import Data.Aeson
import Data.String

import qualified Data.ByteString.Lazy as B

import Control.Monad

import Lrm
import qualified Election as E
import Brute
import qualified Output as O

version = "1.1.1"

main :: IO ()
main = 
    do args <- Env.getArgs
       handleArgs (Write . show) False args
       Exit.exitSuccess

-- Arguments

data Argument
    = Write String
    | Location String
    | Save String B.ByteString
    | Format
    | Fail String

identifyArg :: (O.Output -> Argument) -> Bool -> String -> IO Argument
identifyArg retfunc nec_format arg =
    case arg of
        "--help" ->
            return $ Write $ unlines 
                [ "-------------------------------------------------------------------------------------------------"
                , "Thank You for using this program!"
                , "  x   --help                 Display this message."
                , "  x   --version              Show the version of the program."
                , "  x   --example              Creates an example JSON file."
                , "  x   --nec-format           Tells the program that the file is in New Electoral College format."
                , "                             This is the format used for the JSON files on The New Electoral"
                , "                             College website."
                , "  x   --out=*.json           Writes results to a JSON file."
                , "  x   *.json                 Run program with JSON file."
                , ""
                , "Source:"
                , "  https://github.com/NewElectoralCollege/wasted_vote"
                , ""
                , "Copyright info:"
                , "  Copyright 2021 - The New Electoral College"
                , "-------------------------------------------------------------------------------------------------"
                ]

        "--version" ->
            return $ Write version

        "--example" ->
            return $ Save "example.json"
                "{\n\
                \   \"parties\": [\n\
                \       { \"name\": \"Party A\", \"votes\": 100000 },\n\
                \       { \"name\": \"Party B\", \"votes\": 80000 },\n\
                \       { \"name\": \"Party C\", \"votes\": 30000 },\n\
                \       { \"name\": \"Party D\", \"votes\": 20000 }\n\
                \   ],\n\
                \   \"seats\": 8,\n\
                \   \"quota\": \"hare\"\n\
                \}"

        "--nec-format" ->
            return Format
        
        a ->
            if take 6 a == "--out=" 
                then return $ Location $ drop 6 a
                else if FP.takeExtension a == ".json" then
                    do ri <- E.decodeFileToEither nec_format a
                       case ri of
                            Left s ->
                                return $ Fail ("The following error was returned: " ++ s)
                
                            Right p ->
                                return $ retfunc (brute (E.name p) (E.quota p) (E.parties p) (E.seats p))
                else return $ Fail ("I can't identify this argument: " ++ a)


handleArgs :: (O.Output -> Argument) -> Bool -> [String] -> IO ()
handleArgs _ _ [] = Exit.exitFailure
handleArgs retfunc nec_format (x:xs) = 
    do r <- identifyArg retfunc nec_format x
       case r of
            Write s -> putStrLn s
            Location l -> handleArgs (Save l . encode) nec_format xs
            Save f t -> B.writeFile f t
            Format -> handleArgs retfunc True xs
            Fail f -> do putStrLn f
                         Exit.exitFailure

