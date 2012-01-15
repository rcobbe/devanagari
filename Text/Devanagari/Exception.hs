{-# LANGUAGE DeriveDataTypeable #-}

-- | Defines the exception hierarchy that this library can throw, as well as
-- the specific representation of the 'Control.Monad.Error.Error' monad that we
-- use within the library.
module Text.Devanagari.Exception where

import qualified Control.Monad.Error.Class as CMEC

-- | Exception hierarchy.
data Error
  -- | Used to signal incorrect Unicode input.
  = BadUnicode { input :: String, -- ^ the input that triggered the error
                 msg :: String    -- ^ detailed error message
               }
  -- | Used to signal incorrect Velthuis input.
  | BadVelthuis { input :: String, -- ^ the input that triggered the error
                  msg :: String    -- ^ detailed error message
                }
  -- | Generic error required by the 'CMEC.Error' class.
  | OtherError { msg :: String }

instance CMEC.Error Error where
  noMsg = OtherError "Devanagari error!"
  strMsg s = OtherError s

instance Show Error where
  show (BadUnicode { input = i, msg = m }) =
    "Invalid Unicode input \"" ++ i ++ "\": " ++ m
  show (BadVelthuis { input = i, msg = m }) =
    "Invlid Velthuis input \"" ++ i ++ "\": " ++ m
  show (OtherError msg) =
    "Other Devanagari error: " ++ msg

type Exceptional a = Either Error a
