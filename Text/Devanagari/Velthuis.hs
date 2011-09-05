module Text.Devanagari.Velthuis(toPhonemic, fromPhonemic)
where

-- originally thought I'd need to avoid "n "s for use with Racket/Scribble --
-- but Scribble means that quotes are OK.

import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T
import qualified Text.Devanagari.Phonemic as P

toPhonemic :: String -> Maybe [P.Phoneme]
toPhonemic [] = Just []
toPhonemic s =
  case (T.matchPrefix velthuisTrie s) of
    Nothing -> Nothing
    Just (s', Nothing) -> toPhonemic s'
    Just (s', Just p) -> mcons p (toPhonemic s')

fromPhonemic :: [P.Phoneme] -> String
fromPhonemic ps = fromPhonemic' ps []

-- does phonemic translation, inserting vowelSep before vowel
fromPhonemic' :: [P.Phoneme] -> String -> String
fromPhonemic' [] _ = []
fromPhonemic' (p:ps) vowelSep
  | P.isVowel p = vowelSep ++ velthuis ++ (fromPhonemic' ps "{}")
  | otherwise = velthuis ++ (fromPhonemic' ps "")
  where velthuis =
          case (M.lookup p velthuisMap) of
            Just s -> s
            Nothing -> error "Velthuis.fromPhonemic: internal error"

-- alist defining mapping between velthuis, phonemic.
velthuisAList :: [(String, P.Phoneme)]
velthuisAList =
  addVowels [("k", P.K),
             ("kh", P.Kh),
             ("g", P.G),
             ("gh", P.Gh),
             ("\"n", P.Ng),
             ("c", P.C),
             ("ch", P.Ch),
             ("j", P.J),
             ("jh", P.Jh),
             ("~n", P.PalN),
             (".t", P.RetT),
             (".th", P.RetTh),
             (".d", P.RetD),
             (".dh", P.RetDh),
             (".n", P.RetN),
             ("t", P.T),
             ("th", P.Th),
             ("d", P.D),
             ("dh", P.Dh),
             ("n", P.N),
             ("p", P.P),
             ("ph", P.Ph),
             ("b", P.B),
             ("bh", P.Bh),
             ("m", P.M),
             ("y", P.Y),
             ("r", P.R),
             ("l", P.L),
             ("v", P.V),
             ("\"s", P.PalS),
             (".s", P.RetS),
             ("s", P.S),
             ("h", P.H)]

-- trie from velthuis encoding to phoneme.  Mapping to Nothing means that
-- symbol should be silently discarded from output; we use this to implement
-- {} for hiatus; useful in, say, a{}u or aa{}i.
velthuisTrie :: Trie Char (Maybe P.Phoneme)
velthuisTrie =
  T.fromList (("{}", Nothing) : map (\(v, p) -> (v, Just p)) velthuisAList)

-- Map from phoneme to velthuis encoding
velthuisMap :: Map P.Phoneme String
velthuisMap =
  M.fromList (map (\(v, p) -> (p, v)) velthuisAList)

-- add vowel translation entries to the supplied list; abstracts over various
-- vowel modifiers.  We include long vocalic L, even though it's not in the
-- velthuis mapping that xetex expects, because otherwise we'd have no
-- representation of it in the Velthuis encoding.  Probably not a real concern,
-- as it shouldn't ever arise.
addVowels :: [(String, P.Phoneme)] -> [(String, P.Phoneme)]
addVowels base =
  foldr addVowel base [("a", P.A),
                       ("aa", P.AA),
                       ("i", P.I),
                       ("ii", P.II),
                       ("u", P.U),
                       ("uu", P.UU),
                       (".r", P.VocR),
                       (".R", P.VocRR),
                       (".l", P.VocL),
                       (".L", P.VocLL),
                       ("e", P.E),
                       ("ai", P.AI),
                       ("o", P.O),
                       ("au", P.AU)]

-- add a single vowel, plus all modifiers, to base alist.
addVowel :: (String, P.VowelMod -> P.Phoneme) -> [(String, P.Phoneme)]
            -> [(String, P.Phoneme)]
addVowel (s, vc) base =
  (s, vc P.NoMod) :
  (s ++ ".h", vc P.Visarga) :
  (s ++ ".m", vc P.Anusvara) : base

mcons :: a -> Maybe [a] -> Maybe [a]
mcons _ Nothing = Nothing
mcons x (Just xs) = Just (x:xs)
