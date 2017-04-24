-- Copyright 2012-2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
