-- | Defines the exception hierarchy that this library can throw, plus some
--   utility functions
module Text.Devanagari.Exception(Error(..), userMessage) where

-- | Exception type.
data Error
  -- | Used to signal incorrect Unicode input.
  = BadUnicode { input :: String, -- ^ the input that triggered the error
                 msg :: String    -- ^ detailed error message
               }
  -- | Used to signal incorrect Velthuis input.
  | BadVelthuis { input :: String, -- ^ the input that triggered the error
                  msg :: String    -- ^ detailed error message
                }
    deriving (Show, Eq, Ord)

-- | Produce a message for the exception, suitable for end users
userMessage :: Error -> String
userMessage (BadUnicode { input = i, msg = m }) =
  "Invalid Unicode input \"" ++ i ++ "\": " ++ m
userMessage (BadVelthuis { input = i, msg = m }) =
  "Invalid Velthuis input \"" ++ i ++ "\": " ++ m
