module Main where

import qualified System.Exit as Exit
import qualified System.Environment as Env
import qualified System.FilePath as FP

import Data.Aeson

import qualified Data.ByteString.Lazy as B

import Lrm

version = "0.1"

main :: IO ()
main = 
    do args <- Env.getArgs
       handleArgs args
       Exit.exitSuccess

handleArg :: String -> IO ()
handleArg arg =
    case arg of
        "--help" ->
            putStrLn $ unlines ["Hello", "Goodbye"]

        "--version" ->
            putStrLn version
        
        a ->
            if FP.takeExtension a == ".json" then
                do let file = (take (length a - 5) a)
                   r <- (eitherDecode <$> getJson file) :: IO (Either String Party)
                   putStrLn file


            else

            do putStrLn ("I can't identify this argument: " ++ a)
               Exit.exitFailure

handleArgs :: [String] -> IO ()
handleArgs [] = Exit.exitFailure
handleArgs (x:xs) = handleArg x

-- Json

getJson :: String -> IO B.ByteString 
getJson file = B.readFile file



