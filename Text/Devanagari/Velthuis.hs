module Text.Devanagari.Velthuis(toPhonemic, fromPhonemic)
where

-- originally thought I'd need to avoid "n "s for use with Racket/Scribble --
-- but Scribble means that quotes are OK.

import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T
import qualified Text.Devanagari.Phonemic as P

toPhonemic :: String -> Maybe ([P.Phoneme], String)
toPhonemic [] = Nothing
toPhonemic s = toPhonemicNonempty s

toPhonemicNonempty :: String -> Maybe ([P.Phoneme], String)
toPhonemicNonempty [] = Just ([], "")
toPhonemicNonempty s =
  -- Maybe monad
  do (s', p) <- T.matchPrefix velthuisTrie s
     (p', rest) <- toPhonemicNonempty s'
     case p of
       Nothing -> return (p', rest)
       Just x -> return (x : p', rest)

fromPhonemic :: [P.Phoneme] -> Maybe String
fromPhonemic ps =
  do strs <- mapM (\ p -> M.lookup p velthuisMap) ps
     return $ concat strs

velthuisAList :: [(String, P.Phoneme)]
velthuisAList = [("a", P.A),
                 ("aa", P.AA),
                 ("i", P.I),
                 ("ii", P.II),
                 ("u", P.U),
                 ("uu", P.UU),
                 (".r", P.VocR),
                 (".R", P.VocRR),
                 (".l", P.VocL),
                 ("e", P.E),
                 ("ai", P.AI),
                 ("o", P.O),
                 ("au", P.AU),
                 (".h", P.Visarga),
                 (".m", P.Anusvara),
                 ("k", P.K),
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

velthuisTrie :: Trie Char (Maybe P.Phoneme)
velthuisTrie =
  T.fromList (("{}", Nothing) : map (\(v, p) -> (v, Just p)) velthuisAList)

velthuisMap :: Map P.Phoneme String
velthuisMap =
  M.fromList (map (\(v, p) -> (p, v)) velthuisAList)
