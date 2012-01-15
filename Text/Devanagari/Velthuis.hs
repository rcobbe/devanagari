module Text.Devanagari.Velthuis(toSegments, fromSegments)
where

import Control.Monad.Error

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
  do vowelCtor <- parseStrings [("aa", AA),
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
  parseStrings [("kh", Kh),
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
                (".s", S),
                ("h", H)]
  <?> "Velthuis consonant"

-- | 'parseStrings strs' builds a parser that attempts to parse the strings in
-- the domain of 'strs' in order; on success, returns the value corresponding
-- to the string found.
parseStrings :: [(String, a)] -> GenParser Char st a
parseStrings [] = parserZero
parseStrings ((str, val) : rest) =
  (do string str
      return val)
  <|> parseStrings rest

fromSegments :: [Segment] -> String
fromSegments = undefined

{-

-- originally thought I'd need to avoid "n "s for use with Racket/Scribble --
-- but Scribble means that quotes are OK.

import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T
import qualified Text.Devanagari.Segments as S

toSegments :: String -> Maybe [S.Segment]
toSegments [] = Just []
toSegments str =
  case (T.matchPrefix velthuisTrie str) of
    Nothing -> Nothing
    Just (str', Nothing) -> toSegments str' -- matched {}, so no output
    Just (str', Just seg) -> mcons seg (toSegments str')

fromSegments :: [S.Segment] -> String
fromSegments [] = ""
fromSegments [s] = velthuisMap ! s
fromSegments (seg1 : seg2 : segs) =
  velthuisMap ! seg1 ++
  computeVowelSep seg1 seg2 ++
  fromSegments (seg2 : segs)

computeVowelSep :: S.Segment -> S.Segment -> String
computeVowelSep s1 s2 =
  if requiresVowelSepAfter s1 && requiresVowelSepBefore s2
  then "{}"
  else ""
  where
    -- segment requires {} after it for hiatus
    requiresVowelSepAfter :: S.Segment -> Bool
    requiresVowelSepAfter (S.A S.NoMod) = True
    requiresVowelSepAfter (S.AA S.NoMod) = True
    requiresVowelSepAfter _ = False

    -- segment requires {} before it for hiatus
    requiresVowelSepBefore :: S.Segment -> Bool
    requiresVowelSepBefore (S.I _) = True
    requiresVowelSepBefore (S.II _) = True
    requiresVowelSepBefore (S.U _) = True
    requiresVowelSepBefore (S.UU _) = True
    requiresVowelSepBefore _ = False

-- variant of M.! with a more specific error message
(!) :: Map S.Segment String -> S.Segment -> String
map ! segment =
  case (M.lookup segment map) of
    Just x -> x
    Nothing -> error "Velthuis.fromSegments: internal error"

-- alist defining mapping between velthuis, segmental.
velthuisAList :: [(String, S.Segment)]
velthuisAList =
  addVowels [("k", S.K),
             ("kh", S.Kh),
             ("g", S.G),
             ("gh", S.Gh),
             ("\"n", S.Ng),
             ("c", S.C),
             ("ch", S.Ch),
             ("j", S.J),
             ("jh", S.Jh),
             ("~n", S.PalN),
             (".t", S.RetT),
             (".th", S.RetTh),
             (".d", S.RetD),
             (".dh", S.RetDh),
             (".n", S.RetN),
             ("t", S.T),
             ("th", S.Th),
             ("d", S.D),
             ("dh", S.Dh),
             ("n", S.N),
             ("p", S.P),
             ("ph", S.Ph),
             ("b", S.B),
             ("bh", S.Bh),
             ("m", S.M),
             ("y", S.Y),
             ("r", S.R),
             ("l", S.L),
             ("v", S.V),
             ("\"s", S.PalS),
             (".s", S.RetS),
             ("s", S.S),
             ("h", S.H)]

-- trie from velthuis encoding to segments.  Mapping to Nothing means that
-- symbol should be silently discarded from output; we use this to implement
-- {} for hiatus; useful in, say, a{}u or aa{}i.
velthuisTrie :: Trie Char (Maybe S.Segment)
velthuisTrie =
  T.fromList (("{}", Nothing) : map (\(v, s) -> (v, Just s)) velthuisAList)

-- Map from segment to velthuis encoding
velthuisMap :: Map S.Segment String
velthuisMap =
  M.fromList (map (\(v, s) -> (s, v)) velthuisAList)

-- add vowel translation entries to the supplied list; abstracts over various
-- vowel modifiers.  We include long vocalic L, even though it's not in the
-- velthuis mapping that xetex expects, because otherwise we'd have no
-- representation of it in the Velthuis encoding.  Probably not a real concern,
-- as it shouldn't ever arise.
addVowels :: [(String, S.Segment)] -> [(String, S.Segment)]
addVowels base =
  foldr addVowel base [("a", S.A),
                       ("aa", S.AA),
                       ("i", S.I),
                       ("ii", S.II),
                       ("u", S.U),
                       ("uu", S.UU),
                       (".r", S.VocR),
                       (".R", S.VocRR),
                       (".l", S.VocL),
                       (".L", S.VocLL),
                       ("e", S.E),
                       ("ai", S.AI),
                       ("o", S.O),
                       ("au", S.AU)]

-- add a single vowel, plus all modifiers, to base alist.
addVowel :: (String, S.VowelMod -> S.Segment) -> [(String, S.Segment)]
            -> [(String, S.Segment)]
addVowel (str, vowelCtor) base =
  (str, vowelCtor S.NoMod) :
  (str ++ ".h", vowelCtor S.Visarga) :
  (str ++ ".m", vowelCtor S.Anusvara) : base

mcons :: a -> Maybe [a] -> Maybe [a]
mcons _ Nothing = Nothing
mcons x (Just xs) = Just (x:xs)

-}
