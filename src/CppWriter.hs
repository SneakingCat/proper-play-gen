module CppWriter (
  render
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)
import Control.Monad (mapM_)

type StringWriter = Writer String ()

-- | Wrap the Writer monad
render :: [ModuleDef] -> (String, String)
render moduleDef = (,) "Main.cc" $ (execWriter . renderCpp) moduleDef

renderCpp :: [ModuleDef] -> StringWriter
renderCpp (ModuleDecl moduleName:moduleBody) = do
  mapM_ renderInclude [moduleName ++ ".hh"
                      , "ErlComm.hh"
                      , "ei.h"
                      , "erl_interface.h"
                      , "cstring"
                      , "cassert"]
  tell "\n"
  renderBufSize
  renderPrologue
  renderEpilogue
  
renderInclude :: String -> StringWriter
renderInclude i = tell $ "#include <" ++ i ++ ">\n"

renderBufSize :: StringWriter
renderBufSize = tell "static const int BufSize = 1024;\n\n"

renderPrologue :: StringWriter
renderPrologue = do
  tell "// Communication between Erlang and this program is through\n"
  tell "// stdin and stdout. If tracing is needed from this program it\n"
  tell "// must be written to stderr or a separate file.\n"
  tell "int main() {\n"
  tell "  erl_init(NULL, 0);\n"
  tell "  ErlComm::Byte buf[BufSize];\n"
  tell "  // Receive loop of messages from Erlang\n"
  tell "  while (ErlComm::receive(buf) > 0) {\n\n"
  tell "    // Decode the message tuple\n"
  tell "    ETERM *tuple = erl_decode(buf);\n"
  tell "    assert(ERL_IS_TUPLE(tuple));\n\n"
  tell "    // Pick the first element of the tuple - the function to execute\n"
  tell "    ETERM *func = erl_element(1, tuple);\n"
  tell "    assert(ERL_IS_ATOM(func));\n\n"
  
renderEpilogue :: StringWriter
renderEpilogue = do
  tell "    erl_free_compound(tuple);\n"
  tell "    erl_free_term(func);\n"
  tell "  }\n"
  tell "  return 0;\n"
  tell "}\n"
  