{-# LANGUAGE DeriveDataTypeable #-}

module Text.Devanagari.Exception where

import qualified Control.Exception as Exception
import Data.Typeable

data Exception = BadUnicode { input :: String, msg :: String }
                 deriving (Typeable)

instance Show Exception where
  show (BadUnicode { input = i, msg = m }) =
    "Invalid Unicode input \"" ++ i ++ "\": " ++ m

instance Exception.Exception Exception
