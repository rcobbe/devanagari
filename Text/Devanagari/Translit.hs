module Text.Devanagari.Translit()
       where

import qualified Data.Trie as T
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Unicode as U

type PhonemeTrie = T.Trie Char [P.Phoneme]

-- interface options considered:
-- 1) unicodeToPhonemic :: String -> Maybe [[P.Phoneme]]
--          splits at word boundaries, discards whitespace; None on failure
-- 2) unicodeToPhonemic :: String -> Maybe ([P.Phoneme], String)
--          convert what we can, return the rest; None if no conversion possible
-- 3) unicodeToPhonemic :: String -> ([P.Phoneme], String)
--          convert what we can; return the rest; return ([], arg) on failure
-- going w/ #2; seems clearest
-- no error checking for now.  Examples of things we might check:
--   - only initial vowels in word-initial position
--   - only combining vowels medially/finally
--   - visarga, anusvara must immediately follow vowels
-- ignore word-initial special casing (i.e., only combining forms legal in
--   medial, final position)
-- #1 would have involved incorporating notion of whitespace into this module.
-- Not only is this a loss of modularity & generality, but it's a pain, and I
-- don't know enough to predict the definition of whitespace yet.

toPhonemic :: String -> Maybe ([P.Phoneme], String)
toPhonemic [] = Just ([], [])
toPhonemic s =
  do (s', phonemes) <- T.matchPrefix unicodeToPhonemicTrie s
     (phonemes', rest) <- toPhonemic s'
     return (phonemes ++ phonemes', rest)

unicodeToPhonemicTrie :: T.Trie Char [P.Phoneme]
unicodeToPhonemicTrie =
  foldr addEntries T.empty (vowelEntries : consonantEntries)
  where addEntries :: [(String, [P.Phoneme])] -> PhonemeTrie -> PhonemeTrie
        addEntries entries trie = foldr addEntry trie entries

        addEntry :: (String, [P.Phoneme]) -> PhonemeTrie -> PhonemeTrie
        addEntry (keys, val) trie = T.insert keys val trie

consonantEntries :: [[(String, [P.Phoneme])]]
consonantEntries =
  map makeConsonantEntries [(U.ka, P.K),
                            (U.kha, P.Kh),
                            (U.ga, P.G),
                            (U.gha, P.Gh),
                            (U.velarNa, P.Ng),
                            (U.ca, P.C),
                            (U.cha, P.Ch),
                            (U.ja, P.J),
                            (U.jha, P.Jh),
                            (U.palatalNa, P.PalN),
                            (U.retroTa, P.RetT),
                            (U.retroTha, P.RetTh),
                            (U.retroDa, P.RetD),
                            (U.retroDha, P.RetDh),
                            (U.retroNa, P.RetN),
                            (U.ta, P.T),
                            (U.tha, P.Th),
                            (U.da, P.D),
                            (U.dha, P.Dh),
                            (U.na, P.N),
                            (U.pa, P.P),
                            (U.pha, P.Ph),
                            (U.ba, P.B),
                            (U.bha, P.Bh),
                            (U.ma, P.M),
                            (U.ya, P.Y),
                            (U.ra, P.R),
                            (U.la, P.L),
                            (U.va, P.V),
                            (U.palatalSa, P.PalS),
                            (U.retroSa, P.RetS),
                            (U.sa, P.S),
                            (U.ha, P.H)]

vowelEntries :: [(String, [P.Phoneme])]
vowelEntries = [([U.initA], [P.A]),
                ([U.initAA], [P.AA]),
                ([U.combAA], [P.AA]),
                ([U.initI], [P.I]),
                ([U.combI], [P.I]),
                ([U.initII], [P.II]),
                ([U.combII], [P.II]),
                ([U.initU], [P.U]),
                ([U.combU], [P.U]),
                ([U.initUU], [P.UU]),
                ([U.combUU], [P.UU]),
                ([U.initVocR], [P.VocR]),
                ([U.combVocR], [P.VocR]),
                ([U.initVocRR], [P.VocRR]),
                ([U.combVocRR], [P.VocRR]),
                ([U.initVocL], [P.VocL]),
                ([U.combVocL], [P.VocL]),
                ([U.initVocLL], [P.VocLL]),
                ([U.combVocLL], [P.VocLL]),
                ([U.initE], [P.E]),
                ([U.combE], [P.E]),
                ([U.initAI], [P.AI]),
                ([U.combAI], [P.AI]),
                ([U.initO], [P.O]),
                ([U.combO], [P.O]),
                ([U.initAU], [P.AU]),
                ([U.combAU], [P.AU]),
                ([U.visarga], [P.Visarga]),
                ([U.anusvara], [P.Anusvara])]

makeConsonantEntries :: (Char, P.Phoneme) -> [(String, [P.Phoneme])]
makeConsonantEntries (c, p) =
  ([c, U.virama], [p])
  : ([c], [p, P.A])
  : map (makeConsonantEntry c p) [(U.combAA, P.AA),
                                  (U.combI, P.I),
                                  (U.combII, P.II),
                                  (U.combU, P.U),
                                  (U.combUU, P.UU),
                                  (U.combVocR, P.VocR),
                                  (U.combVocRR, P.VocRR),
                                  (U.combVocL, P.VocL),
                                  (U.combVocLL, P.VocLL),
                                  (U.combE, P.E),
                                  (U.combAI, P.AI),
                                  (U.combO, P.O),
                                  (U.combAU, P.AU)]

makeConsonantEntry :: Char -> P.Phoneme -> (Char, P.Phoneme)
                      -> (String, [P.Phoneme])
makeConsonantEntry consonant consPhoneme (vowel, vowelPhoneme) =
  ([consonant, vowel], [consPhoneme, vowelPhoneme])
