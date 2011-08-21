module Text.Velthuis(velthuisToUnicode, unicodeToVelthuis)
where

import qualified Data.Trie as T
import qualified Text.Devanagari as D

-- Recursive types require a constructor, so we can't do this with "type"
-- instead of "newtype".
newtype DevnagTrie = DevnagTrie (T.Trie Char (String, DevnagTrie))

velthuisToUnicode :: String -> String
velthuisToUnicode s = v2u s initialTrie

v2u :: String -> DevnagTrie -> String
v2u [] _ = []
v2u s@(x:xs) (DevnagTrie t) =
  case T.matchPrefix t s of
    Nothing -> x : v2u xs initialTrie
    Just (remainder, (unicode, newTrie)) ->
      unicode ++ (v2u remainder newTrie)
      -- XXX what is cost model for recursion and ++ ?

unicodeToVelthuis :: String -> String
unicodeToVelthuis = undefined

-- visaraga & anusvara are combining forms that are best thought
-- of as modifications to an immediately-preceding vowel
-- (".h",   (D.visarga,  afterVowelTrie))
-- (".m",   (D.anusvara, afterVowelTrie))

vowels :: [(String, (String, DevnagTrie))]
vowels = [("a",    (D.a,        afterVowelTrie)),
          ("aa",   (D.aa,       afterVowelTrie)),
          ("i",    (D.i,        afterVowelTrie)),
          ("ii",   (D.ii,       afterVowelTrie)),
          ("u",    (D.u,        afterVowelTrie)),
          ("uu",   (D.uu,       afterVowelTrie)),
          (".r",   (D.vocR,     afterVowelTrie)),
          (".r.r", (D.vocRr,    afterVowelTrie)),
          (".l",   (D.vocL,     afterVowelTrie)),
          (".l.l", (D.vocLl,    afterVowelTrie)),
          ("e",    (D.e,        afterVowelTrie)),
          ("ai",   (D.ai,       afterVowelTrie)),
          ("o",    (D.o,        afterVowelTrie)),
          ("au",   (D.au,       afterVowelTrie))]

bareConsonants :: [(String, (String, DevnagTrie))]
bareConsonants = [("k",    (D.k,        afterConsonantTrie)),
                  ("kh",   (D.kh,       afterConsonantTrie)),
                  ("g",    (D.g,        afterConsonantTrie)),
                  ("gh",   (D.gh,       afterConsonantTrie)),
                  ("'n",   (D.velarN,   afterConsonantTrie)),
                  ("c",    (D.c,        afterConsonantTrie)),
                  ("ch",   (D.ch,       afterConsonantTrie)),
                  ("j",    (D.j,        afterConsonantTrie)),
                  ("jh",   (D.jh,       afterConsonantTrie)),
                  ("~n",   (D.palatalN, afterConsonantTrie)),
                  (".t",   (D.retroT,   afterConsonantTrie)),
                  (".th",  (D.retroTh,  afterConsonantTrie)),
                  (".d",   (D.retroD,   afterConsonantTrie)),
                  (".dh",  (D.retroDh,  afterConsonantTrie)),
                  (".n",   (D.retroN,   afterConsonantTrie)),
                  ("t",    (D.t,        afterConsonantTrie)),
                  ("th",   (D.th,       afterConsonantTrie)),
                  ("d",    (D.d,        afterConsonantTrie)),
                  ("dh",   (D.dh,       afterConsonantTrie)),
                  ("n",    (D.n,        afterConsonantTrie)),
                  ("p",    (D.p,        afterConsonantTrie)),
                  ("ph",   (D.ph,       afterConsonantTrie)),
                  ("b",    (D.b,        afterConsonantTrie)),
                  ("bh",   (D.bh,       afterConsonantTrie)),
                  ("m",    (D.m,        afterConsonantTrie)),
                  ("y",    (D.y,        afterConsonantTrie)),
                  ("r",    (D.r,        afterConsonantTrie)),
                  ("l",    (D.l,        afterConsonantTrie)),
                  ("v",    (D.v,        afterConsonantTrie)),
                  ("'s",   (D.palatalS, afterConsonantTrie)),
                  (".s",   (D.retroS,   afterConsonantTrie)),
                  ("s",    (D.s,        afterConsonantTrie)),
                  ("h",    (D.h,        afterConsonantTrie))]

visargaConsonants :: [(String, (String, DevnagTrie))]
visargaConsonants =
  map f bareConsonants
    where f :: (String, (String, DevnagTrie)) -> (String, (String, DevnagTrie))
          f (s1, (s2, t)) = (s1, (D.virama ++ s2, t))

initialTrie :: DevnagTrie
initialTrie =
  DevnagTrie $ T.fromList (vowels ++ bareConsonants)

afterVowelTrie :: DevnagTrie
afterVowelTrie =
  DevnagTrie $ (T.insert "{}" ("", initialTrie) (T.fromList bareConsonants))

afterConsonantTrie :: DevnagTrie
afterConsonantTrie =
  DevnagTrie $ T.fromList (vowels ++ visargaConsonants)
