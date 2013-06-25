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
    (f, ps) = case def of
      (MethodDecl f ps)   -> (f, ps)
      (StaticDecl _ f ps) -> (f, ps)
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
    (f, ps) = case def of
      (MethodDecl f ps)   -> (f, ps)
      (StaticDecl _ f ps) -> (f, ps)
    cs     = map shallConvertToBin ps
  in
   gen (strToLower f) (length cs) cs  
  where
    gen :: String -> Int -> [Bool] -> StringWriter
    gen f l cs = do
      tell $ f ++ "(" ++ formals l ++ ") ->\n"
      tell $ "    ?CppComm:call({" ++ f ++ (cargs cs) ++ "}).\n\n"      
      
    formals :: Int -> String
    formals 0 = ""
    formals 1 = "Arg1"
    formals n = foldl (\a b -> a ++ ",Arg" ++ show b) "Arg1" [2..n]
    
    cargs :: [Bool] -> String
    cargs []     = ""
    cargs (c:cs) = snd $ foldl (\(n, a) c -> (n+1, a ++ maybeConvertArg c n)) (2, (maybeConvertArg c 1)) cs

strToLower :: String -> String
strToLower = map toLower

shallConvertToBin :: Param -> Bool
shallConvertToBin (Ptr _) = False
shallConvertToBin (Value v) =
  case v of
    String    -> True
    otherwise -> False
    
maybeConvertArg :: Bool -> Int -> String
maybeConvertArg False n = ",Arg" ++ show n
maybeConvertArg True n  = ",list_to_binary(Arg" ++ show n ++ ")"