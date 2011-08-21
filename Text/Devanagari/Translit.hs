module Text.Devanagari.Translit()
where

import qualified Data.Trie as T
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Unicode as U

toPhonemic :: String -> Maybe [[P.Phoneme]]
toPhonemic = undefined

-- use Parsec, or hand-roll this?
-- special casing for word-initial
-- word boundaries
-- alternatively, try String -> Maybe ([P.Phoneme], String)
--    go as far as we can & return rest of input
--    leaves word boundaries, etc. to caller
--      variant:  String -> ([P.Phoneme], String)
--        toPhonemic s = ([], s) when no prefix of s is valid encoding
