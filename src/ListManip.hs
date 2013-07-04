module ListManip (
  strToLower
  , allButLast
  ) where

import Data.Char (toLower)

strToLower :: String -> String
strToLower = map toLower

allButLast :: [a] -> [a]
allButLast l = take ((length l) -1) l