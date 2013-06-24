module ModuleParser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>), (*>))

data ModuleDef = ModuleDecl String
               deriving Show

-- Identify and return a module declaration
moduleDecl :: Parser ModuleDef
moduleDecl = ModuleDecl <$> between moduleKW whereKW cppId

-- Identify the 'module' keyword
moduleKW :: Parser ()
moduleKW = spaces *> string "module" *> return ()

-- Identify the 'where' keyword
whereKW :: Parser ()
whereKW = spaces *> string "where" *> return ()

-- Identify and return a C++-style identifier
cppId :: Parser String
cppId = spaces *> ((:) <$> oneOf first <*> many (oneOf cont))
  where
    first = ['a'..'z']++['A'..'Z']++"_"
    cont  = first++['0'..'9']