module ErlangWriter (
  write
  ) where

import Control.Monad.Writer

type StringWriter = Writer String ()

write :: String
write = execWriter writeInternal

writeInternal :: StringWriter
writeInternal = do
  tell "-module(hepp).\n\n"
  tell "-export().\n\n"
  mapM_ writeFunc ["a", "b"]
  
writeFunc :: String -> StringWriter
writeFunc func = do
  tell $ func ++ "() ->\n"
  tell "   ok.\n\n"
