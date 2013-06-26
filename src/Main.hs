module Main (
  main
  ) where

import ModuleParser
import ErlangWriter (render)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)

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
processFile fileName = do
  exist <- doesFileExist fileName
  if exist
    then do
         parsingResult <- parseFromFile fileName
         handleParsingResult parsingResult
    else putStrLn $ "Cannot find file: " ++ fileName
         
handleParsingResult :: Either ParseError [ModuleDef] -> IO ()
handleParsingResult (Left msg) = putStrLn $ show msg
handleParsingResult (Right moduleDef) = 
  let
    (fileName, fileContent) = render moduleDef
  in
   writeFile fileName fileContent

      
