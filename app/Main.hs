module Main where

import qualified System.Exit as Exit
import qualified System.Environment as Env
import qualified System.FilePath as FP

import Data.Aeson
import Data.String

import qualified Data.ByteString.Lazy as B

import Lrm

version = "0.1"

main :: IO ()
main = 
    do args <- Env.getArgs
       handleArgs args
       Exit.exitSuccess

-- Arguments

handleArg :: String -> IO ()
handleArg arg =
    case arg of
        "--help" ->
            putStrLn $ unlines 
                [ "-------------------------------------------------------------------------------"
                , "Thank You for using this program!"
                , "  x   --help                 Display this message."
                , "  x   --version              Show the version of the program."
                , "  x   --example              Creates an example JSON file."
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
            putStrLn version

        "--example" ->
            B.writeFile "example.json" $ fromString
                "[\n\
                \  { \"name\": \"Party A\", \"votes\": 100000 },\n\
                \  { \"name\": \"Party B\", \"votes\": 80000 },\n\
                \  { \"name\": \"Party C\", \"votes\": 30000 },\n\
                \  { \"name\": \"Party D\", \"votes\": 20000 }\n\
                \]"
        
        a ->
            if FP.takeExtension a == ".json" then
                do r <- (eitherDecode <$> getJson a) :: IO (Either String [Party])
                   case r of
                        Left s ->
                           putStrLn ("The following error was returned: " ++ s)
                        
                        Right p ->
                            putStrLn "Hello"


            else

            do putStrLn ("I can't identify this argument: " ++ a)
               Exit.exitFailure

handleArgs :: [String] -> IO ()
handleArgs [] = Exit.exitFailure
handleArgs (x:xs) = handleArg x

-- Json

getJson :: String -> IO B.ByteString 
getJson = B.readFile



