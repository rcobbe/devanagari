module Text.Devanagari.Exception where

import qualified Control.Exception as Exception

data Exception = InvalidCharacterExn Char
                 deriving (Typeable)

instance Show Exception where
  show (InvalidCharacterExn c) =
    "Invalid Unicode character: " ++ [c]

instance Exception.Exception Exception
