{-# LANGUAGE DeriveDataTypeable #-}

module Text.Devanagari.Exception where

import qualified Control.Monad.Error.Class as CMEC

data Error = BadUnicode { input :: String, msg :: String }
           | OtherError { msg :: String }

instance CMEC.Error Error where
  noMsg = OtherError "Devanagari error!"
  strMsg s = OtherError s

instance Show Error where
  show (BadUnicode { input = i, msg = m }) =
    "Invalid Unicode input \"" ++ i ++ "\": " ++ m
  show (OtherError msg) =
    "Other Devanagari error: " ++ msg

type Exceptional a = Either Error a
