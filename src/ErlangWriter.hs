module ErlangWriter (
  render
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)
import Control.Monad (mapM_)

type StringWriter = Writer String ()

-- | Wrap the Writer monad
render :: [ModuleDef] -> (String, String)
render moduleDef@(ModuleDecl moduleName:_) = 
  (,) (strToLower moduleName) ++ ".erl" (execWriter . renderErlang) moduleDef

renderErlang :: [ModuleDef] -> StringWriter
renderErlang (ModuleDecl moduleName:moduleBody) = do
  renderModuleHeading
  renderModuleName $ strToLower moduleName
  renderCommunicatorExports
  renderModuleExports moduleBody
  renderMacroDefinitions
  renderStartStopFunctions
  mapM_ renderFunction moduleBody
  
renderModuleHeading :: StringWriter
renderModuleHeading = 
  tell "%% Module and function names may have been converted to lower case\n"
  
renderModuleName :: String -> StringWriter    
renderModuleName m = tell $ "-module(" ++ m ++ ").\n\n"    
                 
renderCommunicatorExports :: StringWriter                  
renderCommunicatorExports = do
  tell "%% API to start and stop the C++ communicator\n"
  tell "-export([start/0,stop/0]).\n\n"
                  
renderModuleExports :: [ModuleDef] -> StringWriter
renderModuleExports []         = return ()
renderModuleExports (def:defs) = do
  tell "%% API for the system under test\n"
  tell "-export(["
  renderModuleExport def
  mapM_ prependExportWithComma defs
  tell "]).\n\n"
  where
    prependExportWithComma def = tell "," >> renderModuleExport def
                  
renderModuleExport :: ModuleDef -> StringWriter
renderModuleExport def =
  let
    (f, ps) = functionData def
    f'      = strToLower f
    a       = (length ps) -1 -- The ps list also contain the return value
  in                         -- Subtract with one will never be less than zero
   renderExportedFunction f' a
  where
    renderExportedFunction :: String -> Int -> StringWriter
    renderExportedFunction f a = tell $ f ++ "/" ++ show a

renderMacroDefinitions :: StringWriter
renderMacroDefinitions = do
  tell "%% Handy macros for C++ communication\n"
  tell "-define(ExtProg, \"./TestMain\").\n"
  tell "-define(CppComm, cpp_comm).\n\n"
  
renderStartStopFunctions :: StringWriter
renderStartStopFunctions = do
  tell "%% Functions for start and stop\n"
  tell "start() ->\n"
  tell "    ?CppComm:start_link(?ExtProg).\n"
  tell "stop() ->\n"
  tell "    ?CppComm:stop().\n\n"

renderFunction :: ModuleDef -> StringWriter
renderFunction def =
  let
    (f, ps) = functionData def
    -- In Erlang the parameters themselves are not that important,
    -- instead the number of parameters and if a parameter is a
    -- list(char())/String and shall be converted to binary before
    -- sending over the line
    f'      = strToLower f    
    cs      = map isString $ allButLast ps
    l       = length cs
  in
   do
     renderFunctionComment def
     renderFunctionHead f l
     renderFunctionBody f cs
     
renderFunctionComment :: ModuleDef -> StringWriter
renderFunctionComment def = tell $ "%% " ++ show def ++ "\n"

renderFunctionHead :: String -> Int -> StringWriter
renderFunctionHead f l = tell $ f ++ "(" ++ formalArgs l ++ ") ->\n"
  where
    formalArgs :: Int -> String
    formalArgs 0 = ""
    formalArgs 1 = "Arg1"
    formalArgs n = foldl appendFormalArg "Arg1" [2..n]
    
    appendFormalArg :: String -> Int -> String
    appendFormalArg acc n = acc ++ ",Arg" ++ show n

renderFunctionBody :: String -> [Bool] -> StringWriter
renderFunctionBody f cs = 
  tell $ "    ?CppComm:call({" ++ f ++ callArgs cs ++ "}).\n\n"
  where
    callArgs :: [Bool] -> String
    callArgs [] = ""
    callArgs cs = snd $ foldl appendCallArg (1, "") cs
    
    appendCallArg :: (Int, String) -> Bool -> (Int, String)
    appendCallArg (n, s) b = (n+1, s ++ assemblyCallArg n b)
    
    assemblyCallArg :: Int -> Bool -> String
    assemblyCallArg n False = ",Arg" ++ show n
    assemblyCallArg n True  = ",list_to_binary(Arg" ++ show n ++ ")"

isString :: Param -> Bool
isString (Ptr _) = False
isString (Value v) =
  case v of
    String    -> True
    otherwise -> False

strToLower :: String -> String
strToLower = map toLower

allButLast :: [a] -> [a]
allButLast l = take ((length l) -1) l

functionData :: ModuleDef -> (String, [Param])
functionData (MethodDecl f ps)   = (f, ps)
functionData (StaticDecl _ f ps) = (f, ps)