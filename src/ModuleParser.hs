module ModuleParser (
  DataType (..)
  , Param (..)
  , ModuleDef (..)
  , parse
  , parseFromFile
  , module Text.ParserCombinators.Parsec
  ) where

import Text.ParserCombinators.Parsec hiding (parse, parseFromFile)
import qualified Text.ParserCombinators.Parsec as P
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

parseFromFile :: String -> IO (Either ParseError [ModuleDef])
parseFromFile fileName = P.parseFromFile parse fileName

parse :: Parser [ModuleDef]
parse = (:) <$> moduleDecl <*> many1 bodyFragment

moduleDecl :: Parser ModuleDef
moduleDecl = ModuleDecl <$> between moduleKW whereKW cppId

bodyFragment :: Parser ModuleDef
bodyFragment = try staticDecl
               <|> try methodDecl
               <?> "Method or static declaration"

methodDecl :: Parser ModuleDef
methodDecl = MethodDecl <$> cppId <*> paramList

staticDecl :: Parser ModuleDef
staticDecl = StaticDecl <$> cppId <*> (char '.' *> cppId) <*> paramList

paramList :: Parser [Param]
paramList = between doubleColon semiColon params

moduleKW :: Parser ()
moduleKW = spaces *> string "module" *> return ()

whereKW :: Parser ()
whereKW = spaces *> string "where" *> return ()

cppId :: Parser String
cppId = spaces *> ((:) <$> oneOf first <*> many (oneOf cont))
  where
    first = ['a'..'z']++['A'..'Z']++"_"
    cont  = first++['0'..'9']
    
doubleColon :: Parser ()
doubleColon = spaces *> string "::" *> return ()

semiColon :: Parser ()
semiColon = spaces *> char ';' *> return ()

arrow :: Parser ()
arrow = spaces *> string "->" *> return ()

params :: Parser [Param]
params = (:) <$> param <*> many (arrow *> param)

param :: Parser Param
param = try ptr
        <|> try value
        <?> "Pointer or value"
        
ptr :: Parser Param
ptr = Ptr <$> (spaces *> char '^' *> dataType)

value :: Parser Param
value = Value <$> (spaces *> dataType)

dataType :: Parser DataType
dataType = try typeVoid
           <|> try typeString
           <|> try typeInteger
           <|> try typeUserDef
           <?> "Void, String, Integer or user defined type"
           
typeVoid :: Parser DataType
typeVoid = spaces *> string "Void" *> return Void

typeString :: Parser DataType
typeString = spaces *> string "String" *> return String

typeInteger :: Parser DataType
typeInteger = spaces *> string "Integer" *> return Integer

typeUserDef :: Parser DataType
typeUserDef = UserDef <$> cppId
