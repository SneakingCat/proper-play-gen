module ErlangWriter (
  generate
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)
import Control.Monad (mapM_)

type StringWriter = Writer String ()

generate :: [ModuleDef] -> String
generate = execWriter . generateErlang

generateErlang :: [ModuleDef] -> StringWriter
generateErlang (ModuleDecl moduleName:moduleBody) = do
  genModuleHeading
  genModuleName $ strToLower moduleName
  genCommunicatorExports
  genModuleExports moduleBody
  genMacroDefinitions
  genStartStopFunctions
  mapM_ genFunction moduleBody
  
genModuleHeading :: StringWriter
genModuleHeading = 
  tell "%% Module and function names may have been converted to lower case\n"
  
genModuleName :: String -> StringWriter    
genModuleName m = tell $ "-module(" ++ m ++ ").\n\n"    
                 
genCommunicatorExports :: StringWriter                  
genCommunicatorExports = do
  tell "%% API to start and stop the C++ communicator\n"
  tell "-export([start/0,stop/0]).\n\n"
                  
genModuleExports :: [ModuleDef] -> StringWriter
genModuleExports []         = return ()
genModuleExports (def:defs) = do
  tell "%% API for the system under test\n"
  tell "-export(["
  genModuleExport def
  mapM_ prependExportWithComma defs
  tell "]).\n\n"
  where
    prependExportWithComma def = tell "," >> genModuleExport def
                  
genModuleExport :: ModuleDef -> StringWriter
genModuleExport def =
  let
    (f, ps) = functionData def
  in
   gen (strToLower f) (length ps)
  where
    gen f ps = tell $ f ++ "/" ++ show ps

genMacroDefinitions :: StringWriter
genMacroDefinitions = do
  tell "%% Handy macros for C++ communication\n"
  tell "-define(ExtProg, \"./TestMain\").\n"
  tell "-define(CppComm, cpp_comm).\n\n"
  
genStartStopFunctions :: StringWriter
genStartStopFunctions = do
  tell "%% Functions for start and stop\n"
  tell "start() ->\n"
  tell "    ?CppComm:start_link(?ExtProg).\n"
  tell "stop() ->\n"
  tell "    ?CppComm:stop().\n\n"

genFunction :: ModuleDef -> StringWriter
genFunction def =
  let
    (f, ps) = functionData def
    -- In Erlang the parameters themselves are not that important,
    -- instead the number of parameters and if a parameter is a
    -- list(char())/String and shall be converted to binary before
    -- sending over the line
    f'      = strToLower f    
    cs      = map isString ps
    l       = length ps
  in
   do
     genFunctionHead f l
     genFunctionBody f cs
     
genFunctionHead :: String -> Int -> StringWriter
genFunctionHead f l = tell $ f ++ "(" ++ formalArgs l ++ ") ->\n"
  where
    formalArgs :: Int -> String
    formalArgs 0 = ""
    formalArgs 1 = "Arg1"
    formalArgs n = foldl appendFormalArg "Arg1" [2..n]
    
    appendFormalArg :: String -> Int -> String
    appendFormalArg acc n = acc ++ ",Arg" ++ show n

genFunctionBody :: String -> [Bool] -> StringWriter
genFunctionBody f cs = tell $ "    ?CppComm({" ++ f ++ callArgs cs ++ "}).\n\n"
  where
    callArgs :: [Bool] -> String
    callArgs [] = ""
    callArgs cs = snd $ foldl appendCallArg (1, "") cs
    
    appendCallArg :: (Int, String) -> Bool -> (Int, String)
    appendCallArg (n, s) b = (n+1, s ++ renderCallArg n b)
    
    renderCallArg :: Int -> Bool -> String
    renderCallArg n False = ",Arg" ++ show n
    renderCallArg n True  = ",list_to_binary(Arg" ++ show n ++ ")"

isString :: Param -> Bool
isString (Ptr _) = False
isString (Value v) =
  case v of
    String    -> True
    otherwise -> False

strToLower :: String -> String
strToLower = map toLower

functionData :: ModuleDef -> (String, [Param])
functionData (MethodDecl f ps)   = (f, ps)
functionData (StaticDecl _ f ps) = (f, ps)