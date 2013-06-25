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
  genModuleExports moduleBody
  
genModuleHeading :: StringWriter
genModuleHeading = 
  tell "%% Module and function names may have been converted to lower case\n"
  
genModuleName :: String -> StringWriter    
genModuleName m = tell $ "-module(" ++ m ++ ").\n\n"    
                 
genModuleExports :: [ModuleDef] -> StringWriter
genModuleExports []         = return ()
genModuleExports (def:defs) = do
  tell "-export(["
  genModuleExport def
  mapM_ prependExportWithComma defs
  tell "]).\n\n"
  where
    prependExportWithComma def = tell "," >> genModuleExport def
                  
genModuleExport :: ModuleDef -> StringWriter
genModuleExport def =
  case def of
    (MethodDecl m p)   -> gen m p
    (StaticDecl _ m p) -> gen m p
  where
    gen m p = tell $ (strToLower m) ++ "/" ++ show (length p)

strToLower :: String -> String
strToLower = map toLower
