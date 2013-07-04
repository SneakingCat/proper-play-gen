module CppWriter (
  render
  ) where

import ModuleParser (DataType(..), Param(..), ModuleDef(..))
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (toLower)
import Control.Monad (mapM_, liftM)

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
  mapM_ renderMsgReception moduleBody
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
  tell "    "
  
renderMsgReception :: ModuleDef -> StringWriter  
renderMsgReception moduleDef =
  let
    f  = funcName moduleDef -- f will be lower case
    as = drop 1 $ allButLast $ params moduleDef
    rt = returnType moduleDef
  in 
   do
     tell $ "if (ErlComm::atomEqualsTo(func, \"" ++ f ++ "\")) {\n"
     renderMsgComment moduleDef
     maybeRenderObjPtrAssignment moduleDef
     maybeRenderReturnValueAssignment rt
     renderCallSite moduleDef
     renderFormalArguments as
     renderReturnMessage rt
     tell "    } else "   
  
renderMsgComment :: ModuleDef -> StringWriter   
renderMsgComment def = tell $ "      // " ++ show def ++ "\n"
   
maybeRenderObjPtrAssignment :: ModuleDef -> StringWriter
maybeRenderObjPtrAssignment (MethodDecl _ ps) =
  let
    t  = ps !! 0
    s  = typeAsStr t
  in
   tell $ "      " ++ s ++ "obj = ErlComm::ptrFromIntegral<"
   ++ s ++ ">(erl_element(2, tuple));\n"
maybeRenderObjPtrAssignment _ = return ()
   
maybeRenderReturnValueAssignment :: Param -> StringWriter
maybeRenderReturnValueAssignment (Value Void) =
  tell "      "
maybeRenderReturnValueAssignment p =
  tell $ "      " ++ typeAsStr p ++ " ret = "

renderCallSite :: ModuleDef -> StringWriter
renderCallSite (StaticDecl m f _) = tell $ m ++ "::" ++ f
renderCallSite (MethodDecl f _)   = tell $ "obj->" ++ f
       
renderFormalArguments :: [Param] -> StringWriter
renderFormalArguments [] = tell "();\n"
renderFormalArguments [a] = tell $ "(" ++ depictArgument 3 a ++ ");\n"
renderFormalArguments (a:as) = 
  tell $ "("
  ++ (snd $ foldl appendArgument (4, depictArgument 3 a) as) ++ ");\n"
  where
    appendArgument (n, s) p = (n+1, s ++ ", " ++ depictArgument n p)

depictArgument :: Int -> Param -> String
depictArgument n t@(Ptr _) = "ErlComm::ptrFromIntegral<" 
                             ++ typeAsStr t 
                             ++ ">(erl_element(" ++ show n ++ ", tuple))"
depictArgument n (Value String) = "ErlComm::fromBinary(erl_element("
                                  ++ show n ++ ", tuple))"
depictArgument _ _ = error "Cannot handle this type"

renderReturnMessage :: Param -> StringWriter
renderReturnMessage (Value Void) = do
  tell "      ETERM *ok = erl_mk_atom(\"ok\");\n"
  renderReturnMessageEpilogue "ok"
renderReturnMessage (Value Integer) = do
  tell "      ETERM *anInt = erl_mk_int(ret);\n"
  renderReturnMessageEpilogue "anInt"
renderReturnMessage (Value _) = error "Cannot handle this type"
renderReturnMessage (Ptr _) = do
  tell "      ETERM *ptr =\n"
  tell "        erl_mk_ulonglong(reinterpret_cast<unsigned long long>(ret));\n"
  renderReturnMessageEpilogue "ptr"

renderReturnMessageEpilogue :: String -> StringWriter
renderReturnMessageEpilogue t = do
  tell $ "      erl_encode(" ++ t ++ ", buf);\n"
  tell $ "      ErlComm::send(buf, erl_term_len(" ++ t ++ "));\n"
  tell $ "      erl_free_term(" ++ t ++ ");\n"

renderEpilogue :: StringWriter
renderEpilogue = do
  tell " {\n" -- This parentesis just will follow an if {} else
  tell "      // Unknown message\n"
  tell "      assert(false);\n"
  tell "    }\n\n"
  tell "    erl_free_compound(tuple);\n"
  tell "    erl_free_term(func);\n"
  tell "  }\n"
  tell "  return 0;\n"
  tell "}\n"
  
funcName :: ModuleDef -> String
funcName (MethodDecl funcName _)   = strToLower funcName
funcName (StaticDecl _ funcName _) = strToLower funcName

params :: ModuleDef -> [Param]
params (MethodDecl _ p)   = p
params (StaticDecl _ _ p) = p

allButLast :: [a] -> [a]
allButLast l = take ((length l) -1) l

returnType :: ModuleDef -> Param
returnType (MethodDecl _ p)   = last p
returnType (StaticDecl _ _ p) = last p

typeAsStr :: Param -> String
typeAsStr (Value t) =
  case t of
    Integer   -> "int"
    String    -> "std::string"
    Void      -> "void"
    UserDef u -> u
typeAsStr (Ptr t) =
  case t of
    Integer   -> "int *"
    String    -> "char *"
    Void      -> "void *"
    UserDef u -> u ++ " *"

strToLower :: String -> String
strToLower = map toLower