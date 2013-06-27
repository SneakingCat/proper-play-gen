module CppWriter (
  render
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)

type StringWriter = Writer String ()

-- | Wrap the Writer monad
render :: [ModuleDef] -> (String, String)
render moduleDef = (,) "Main.cc" $ (execWriter . renderCpp) moduleDef

renderCpp :: [ModuleDef] -> StringWriter
renderCpp (ModuleDecl moduleName:moduleBody) = do
  renderModuleInclude moduleName
  
renderModuleInclude :: String -> StringWriter
renderModuleInclude m = tell $ "#include \"" ++ (m ++ ".hh") ++ "\"\n"