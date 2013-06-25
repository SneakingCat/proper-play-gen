module ErlangWriter (
  generate
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)

type StringWriter = Writer String ()

generate :: [ModuleDef] -> String
generate = execWriter . generateErlang

generateErlang :: [ModuleDef] -> StringWriter
generateErlang (ModuleDecl moduleName:moduleBody) =
  tell moduleName
