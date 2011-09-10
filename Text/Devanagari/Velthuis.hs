module Text.Devanagari.Velthuis(toSegments, fromSegments)
where

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
  where requiresVowelSepAfter (S.A S.NoMod) = True
        requiresVowelSepAfter (S.AA S.NoMod) = True
        requiresVowelSepAfter _ = False
        requiresVowelSepBefore (S.I _) = True
        requiresVowelSepBefore (S.II _) = True
        requiresVowelSepBefore (S.U _) = True
        requiresVowelSepBefore (S.UU _) = True
        requiresVowelSepBefore _ = False

(!) :: Map S.Segment String -> S.Segment -> String
map ! segment =
  case (M.lookup segment map) of
    Just x -> x
    Nothing -> error "Velthuis.fromSegments: internal error"

-- alist defining mapping between velthuis, phonemic.
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

-- trie from velthuis encoding to phoneme.  Mapping to Nothing means that
-- symbol should be silently discarded from output; we use this to implement
-- {} for hiatus; useful in, say, a{}u or aa{}i.
velthuisTrie :: Trie Char (Maybe S.Segment)
velthuisTrie =
  T.fromList (("{}", Nothing) : map (\(v, p) -> (v, Just p)) velthuisAList)

-- Map from phoneme to velthuis encoding
velthuisMap :: Map S.Segment String
velthuisMap =
  M.fromList (map (\(v, p) -> (p, v)) velthuisAList)

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
addVowel (s, vc) base =
  (s, vc S.NoMod) :
  (s ++ ".h", vc S.Visarga) :
  (s ++ ".m", vc S.Anusvara) : base

mcons :: a -> Maybe [a] -> Maybe [a]
mcons _ Nothing = Nothing
mcons x (Just xs) = Just (x:xs)
