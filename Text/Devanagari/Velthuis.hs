module Text.Devanagari.Velthuis(toSegments, fromSegments)
where

import Control.Monad.Error
import Data.List (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

import Text.Devanagari.Exception
import Text.Devanagari.Segments

-- XXX restrict {} to appear only before vowels?  Before i & u?

-- | Converts a Velthuis string to a list of segments.  On error, throws
-- 'BadVelthuis'.
toSegments :: String -> Exceptional [Segment]
toSegments s =
  case parse velthuisWord "" s of
    Left error ->
      case (errorMessages error) of
        [] -> throwError $ BadVelthuis s "unknown error"
        (msg : _) -> throwError $ BadVelthuis s (messageString msg)
    Right segments -> return segments

-- | Parse a word containing at least one Velthuis segment, possibly with a
-- trailing {}.
velthuisWord :: GenParser Char st [Segment]
velthuisWord =
  do word <- many1 velthuisSegment
     optional (string "{}")
     eof                        -- make sure no bogus suffix
     return word
  <?> "Velthuis word"

-- | Parse a single Velthuis segment, with an optional {}.
velthuisSegment :: GenParser Char st Segment
velthuisSegment =
  do optionMaybe (string "{}")
     (parseVowel <|> parseConsonant)
  <?> "Velthuis segment"

-- | Parse a Velthuis vowel, with optional visarga or anusvara.
parseVowel :: GenParser Char st Segment
parseVowel =
  do vowelCtor <- parseStrings vowelMapping
     mod <- vowelModifier
     return $ vowelCtor mod

-- | Parse a Velthuis vowel modifier (visarga or anusvara)
vowelModifier :: GenParser Char st VowelMod
vowelModifier =
  try (string ".h" >> return Visarga)
  <|> try (string ".m" >> return Anusvara)
  <|> return NoMod
  <?> "Velthuis vowel modifier"

-- | Parse a Velthuis consonant.
parseConsonant :: GenParser Char st Segment
parseConsonant =
  parseStrings consonantMapping
  <?> "Velthuis consonant"

-- | 'parseStrings strs' builds a parser that attempts to parse the strings in
-- the domain of 'strs' in order; on success, returns the value corresponding
-- to the string found.
parseStrings :: [(String, a)] -> GenParser Char st a
parseStrings [] = parserZero
parseStrings ((str, val) : rest) =
  (do try (string str)
      return val)
  <|> parseStrings rest

fromSegments :: [Segment] -> String
fromSegments [] = ""
fromSegments [s] = segmentMap ! s
fromSegments (s1 : rest@(s2 : _)) =
  segmentMap ! s1
  ++ vowelSep s1 s2
  ++ fromSegments rest

-- | Maps 'Segment's to corresponding Velthuis strings; does not account for
-- vowel hiatus.
segmentMap :: Map Segment String
segmentMap =
  let consonantMap = Map.fromList (map invert consonantMapping)
      addVowel :: Map Segment String -> (String, VowelMod -> Segment)
                  -> Map Segment String
      addVowel m (str, vowelCtor) =
        Map.insert (vowelCtor NoMod) str
        (Map.insert (vowelCtor Visarga) (str ++ ".h")
         (Map.insert (vowelCtor Anusvara) (str ++ ".m") m))
  in foldl' addVowel consonantMap vowelMapping
  where invert (x, y) = (y, x)

-- | Computes the vowel separator required between the given 'Segment's.
-- Returns either "" or "{}".
vowelSep :: Segment -> Segment -> String
vowelSep (A NoMod) (A _) = "{}"
vowelSep (A NoMod) (AA _) = "{}"
vowelSep (A NoMod) (I _) = "{}"
vowelSep (A NoMod) (II _) = "{}"
vowelSep (A NoMod) (U _) = "{}"
vowelSep (A NoMod) (UU _) = "{}"
vowelSep (AA NoMod) (A _) = "{}"
vowelSep (AA NoMod) (AA _) = "{}"
vowelSep (AA NoMod) (I _) = "{}"
vowelSep (AA NoMod) (II _) = "{}"
vowelSep (AA NoMod) (U _) = "{}"
vowelSep (AA NoMod) (UU _) = "{}"
vowelSep (I NoMod) (I _) = "{}"
vowelSep (I NoMod) (II _) = "{}"
vowelSep (II NoMod) (I _) = "{}"
vowelSep (II NoMod) (II _) = "{}"
vowelSep (U NoMod) (U _) = "{}"
vowelSep (U NoMod) (UU _) = "{}"
vowelSep (UU NoMod) (U _) = "{}"
vowelSep (UU NoMod) (UU _) = "{}"
vowelSep _ _ = ""

-- | Defines the mapping between Velthuis strings and consonant 'Segment's.
-- Order is significant: no key (string) can be a prefix of a later key.
consonantMapping :: [(String, Segment)]
consonantMapping = [("kh", Kh),
                    ("k", K),
                    ("gh", Gh),
                    ("g", G),
                    ("\"n", Ng),
                    ("ch", Ch),
                    ("c", C),
                    ("jh", Jh),
                    ("j", J),
                    ("~n", PalN),
                    (".th", RetTh),
                    (".t", RetT),
                    (".dh", RetDh),
                    (".d", RetD),
                    (".n", RetN),
                    ("th", Th),
                    ("t", T),
                    ("dh", Dh),
                    ("d", D),
                    ("n", N),
                    ("ph", Ph),
                    ("p", P),
                    ("bh", Bh),
                    ("b", B),
                    ("m", M),
                    ("y", Y),
                    ("r", R),
                    ("l", L),
                    ("v", V),
                    ("\"s", PalS),
                    (".s", RetS),
                    ("s", S),
                    ("h", H)]

-- | Defines the mapping between Velthuis strings and vowel 'Segment's.
-- Order is significant: no key (string) can be a prefix of a later key.
vowelMapping :: [(String, VowelMod -> Segment)]
vowelMapping = [("aa", AA),
                ("ai", AI),
                ("au", AU),
                ("ii", II),
                ("uu", UU),
                ("a", A),
                ("i", I),
                ("u", U),
                (".r", VocR),
                (".R", VocRR),
                (".l", VocL),
                (".L", VocLL),
                ("e", E),
                ("o", O)]
