module ModuleParser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>), (*>))

data DataType = Void
              | String
              | Integer
              | UserDef String
              deriving Show
                     
data Param = Value DataType
           | Ptr DataType
           deriving Show

data ModuleDef = ModuleDecl String
               | MethodDecl String [Param]
               | StaticDecl String String [Param]
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
methodDecl = MethodDecl <$> cppId <*> paramList

-- Identify and return a static declaration
staticDecl :: Parser ModuleDef
staticDecl = StaticDecl <$> cppId <*> (char '.' *> cppId) <*> paramList

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

-- Identify a ';'
semiColon :: Parser ()
semiColon = spaces *> char ';' *> return ()

-- Identify a '->'
arrow :: Parser ()
arrow = spaces *> string "->" *> return ()

-- Identify a list of parameters
paramList :: Parser [Param]
paramList = between doubleColon semiColon params

-- Return a sequence of parameters
params :: Parser [Param]
params = (:) <$> param <*> many (arrow *> param)

-- Identify and return a parameter
param :: Parser Param
param = try ptr
        <|> try value
        <?> "Pointer or value"
        
-- Identify and return a pointer
ptr :: Parser Param
ptr = Ptr <$> (spaces *> char '^' *> dataType)

-- Identify and return a value
value :: Parser Param
value = Value <$> (spaces *> dataType)

-- Identify and return a data type
dataType :: Parser DataType
dataType = try typeVoid
           <|> try typeString
           <|> try typeInteger
           <|> try typeUserDef
           <?> "Void, String, Integer or user defined type"
           
-- Identify and return a Void type
typeVoid :: Parser DataType
typeVoid = spaces *> string "Void" *> return Void

-- Identify and return a String type
typeString :: Parser DataType
typeString = spaces *> string "String" *> return String

-- Identify and return an Integer type
typeInteger :: Parser DataType
typeInteger = spaces *> string "Integer" *> return Integer

-- Identify and return a user defined type
typeUserDef :: Parser DataType
typeUserDef = UserDef <$> cppId
