module ModuleParser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>), (*>))

data ModuleDef = ModuleDecl String
               | MethodDecl String
               | StaticDecl String String
               deriving Show

-- Parse a module
parse :: Parser [ModuleDef]
parse = (:) <$> moduleDecl <*> many1 moduleBody

-- Identify and return a module declaration
moduleDecl :: Parser ModuleDef
moduleDecl = ModuleDecl <$> between moduleKW whereKW cppId

-- Identify and return a fragment of a module body
moduleBody :: Parser ModuleDef
moduleBody = try staticDecl
             <|> try methodDecl
             <?> "Method or static declaration"

-- Identify and return a method declaration
methodDecl :: Parser ModuleDef
methodDecl = MethodDecl <$> (spaces *> cppId)

-- Identify and return a static declaration
staticDecl :: Parser ModuleDef
staticDecl = StaticDecl <$> (spaces *> cppId) <*> (char '.' *> cppId)

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
    
-- Identify a '::'
doubleColon :: Parser ()
doubleColon = spaces *> string "::" *> return ()