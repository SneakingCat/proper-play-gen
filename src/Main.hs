module Main (
  main
  ) where

import ModuleParser (ModuleDef, parse)
import ErlangWriter (render)
import System.Environment (getArgs, getProgName)

data UserCommand = Parse String
                 | Help

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (Parse fileName) -> processFile fileName
    Just Help             -> help
    Nothing               -> help
  return ()

help :: IO ()
help = do
  progName <-getProgName
  putStrLn $ "Usage: " ++ progName ++ " [options]"
  putStrLn "Options:"
  putStrLn "-f <filename>    Process the specified file"
  putStrLn "-h               This help screen"

parseArgs :: [String] -> Maybe UserCommand
parseArgs []               = Nothing
parseArgs ["-f", fileName] = Just (Parse fileName)
parseArgs ["-h"]           = Just Help
parseArgs _                = Nothing

processFile :: String -> IO ()
processFile _ = return ()
      
